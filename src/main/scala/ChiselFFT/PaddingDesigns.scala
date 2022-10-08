package ChiselFFT
import chisel3._
import Chisel.log2Ceil
import ChiselFFT.DFTDesigns.DFT_Symmetric_NRV_v2
import chisel3.util._

import scala.collection.mutable
import ComplexModules.FPComplex._
import IEEEConversions.FPConvert._
import ChiselFFT.FFTMRDesigns.{FFT_MixedRadix_Streaming, FFT_MixedRadix_Streaming_v2}
import ChiselFFT.FFTSRDesigns.{FFT_SingleRadix_Streaming_NRO, FFT_SingleRadix_Streaming_NRO_v2}
import ChiselFFT.PermutationDesigns._
import ChiselFFT.TwidFactorDesigns.TwiddleFactorsStreamed_mr_v2
import chisel3.tester._
import chisel3.tester.RawTester.test

import java.io.PrintWriter
import FloatingPointDesigns.FPArithmetic._


object PaddingDesigns {
  class SRAM_Block_streaming(entries: Int, streamingwidth: Int, bw: Int) extends Module {
    val io = IO(new Bundle(){
      val in_en = Input(Bool())
      val in_rst = Input(Bool())
      val in_write_en = Input(Bool())
      val in_offset = Input(UInt(log2Ceil(streamingwidth).W))
      val in_addr = Input(UInt(log2Ceil(entries/streamingwidth).W))
      val in_data = Input(Vec(streamingwidth, new ComplexNum(bw)))
      val out_data = Output(Vec(streamingwidth, new ComplexNum(bw)))
    })
    val entries_per_ram = entries/streamingwidth
    val sram_mems = for (i <- 0 until streamingwidth) yield {
      SyncReadMem(entries_per_ram, new ComplexNum(bw))
    }
    val save_out = RegInit(VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw))))
    when(io.in_rst){ // clear the memory if reset (might move to within the enable statement)
      for(i <- 0 until entries/streamingwidth) {
        for (j <- 0 until streamingwidth) {
          sram_mems(j)(i) := 0.U.asTypeOf(new ComplexNum(bw))
        }
      }
      io.out_data := VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw)))
      save_out := VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw)))
    }.elsewhen(io.in_en) {
      when(io.in_write_en) {
        for (i <- 0 until streamingwidth) {
          when(i.U >= io.in_offset) {
            sram_mems(i)(io.in_addr) := io.in_data(i.U - io.in_offset)
          }
          when(i.U < io.in_offset) {
            sram_mems(i)(io.in_addr + 1.U) := io.in_data(i.U + io.in_offset)
          }
        }
        io.out_data := save_out
      }.otherwise {
        for (i <- 0 until streamingwidth) {
          io.out_data(i) := sram_mems(i)(io.in_addr)
          save_out(i) := sram_mems(i)(io.in_addr)
        }
      }
    }.otherwise{
      io.out_data := save_out
    }
  }

  class RAM_Block_streaming_v2(entries: Int, streamingwidth: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in_en = Input(Bool())
      val in_rst = Input(Bool())
      val in_write_en = Input(Bool())
      val in_offset = Input(UInt(log2Ceil(streamingwidth).W))
      val in_addr = Input(UInt(log2Ceil(entries / streamingwidth).W))
      val in_data = Input(Vec(streamingwidth, new ComplexNum(bw)))
      val out_data = Output(Vec(streamingwidth, new ComplexNum(bw)))
    })
    val entries_per_ram = entries / streamingwidth
    val sram_mems_2 = RegInit(VecInit.fill(2)(VecInit.fill(entries_per_ram)(0.U.asTypeOf(new ComplexNum(bw)))))
    val save_out = RegInit(VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw))))
    when(io.in_rst) { // clear the memory if reset (might move to within the enable statement)
      for (i <- 0 until entries / streamingwidth) {
        for (j <- 0 until streamingwidth) {
          sram_mems_2(j)(i) := 0.U.asTypeOf(new ComplexNum(bw))
        }
      }
      io.out_data := VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw)))
      save_out := VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw)))
    }.elsewhen(io.in_en) {
      when(io.in_write_en) {
        for (i <- 0 until streamingwidth) {
          when(i.U >= io.in_offset) {
            sram_mems_2(i)(io.in_addr) := io.in_data(i.U - io.in_offset)
          }
          when(i.U < io.in_offset) {
            sram_mems_2(i)(io.in_addr + 1.U) := io.in_data(i.U + io.in_offset)
          }
        }
        io.out_data := save_out
      }.otherwise {
        for (i <- 0 until streamingwidth) {
          io.out_data(i) := save_out(i)
          save_out(i) := sram_mems_2(i)(io.in_addr)
        }
      }
    }.otherwise {
      io.out_data := save_out
    }
  }

  class one_dimensional_padding_streaming(paddinglength: Int, streamingwidth: Int, segment_lengths: Array[Int], bw: Int) extends Module{
    val io = IO(new Bundle{
      val in = Input(Vec(streamingwidth, new ComplexNum(bw)))
      val in_en = Input(Bool())
      val in_valid = Input(Bool())
      val out = Output(Vec(streamingwidth, new ComplexNum(bw)))
      val out_en = Output(Bool())
      val out_valid = Output(Bool())
    })
    val ccs_per_segment = for(i <- 0 until segment_lengths.length) yield{
      val c = segment_lengths(i)/streamingwidth
      if(segment_lengths(i)%streamingwidth != 0)
        c+1
      else
        c
    }
    val offset_per_segment = for(i <- 0 until segment_lengths.length) yield{
      val l = ((paddinglength - segment_lengths(i)) / 2)%streamingwidth
      l
    }
    val startind_per_segment = for(i <- 0 until segment_lengths.length) yield{
      val l = ((paddinglength - segment_lengths(i)) / 2)/streamingwidth
      l
    }
    println(s"ccs_per_segment: ${ccs_per_segment}")
    println(s"offset_per_segment: ${offset_per_segment}")
    println(s"startind_per_segment: ${startind_per_segment}")
    println("______________________________________________")
    val offs_seg = Module(new constants_ROM(offset_per_segment.toArray, streamingwidth)).io
    val ccs_seg = Module(new constants_ROM(ccs_per_segment.toArray, ccs_per_segment.max + 1)).io
    val sind_seg = Module(new constants_ROM(startind_per_segment.toArray, paddinglength/streamingwidth)).io
    val cnt_ccs = RegInit(0.U(log2Ceil(ccs_per_segment.max + 1).W))
    val cnt_stg = RegInit(0.U((log2Ceil(segment_lengths.length) + 1).W))

    val switchModes = RegInit(false.B) // indicates when to alternate read/write modes in buffer

    val SRAM_Buffers = VecInit.fill(2)(Module(new RAM_Block_streaming_v2(paddinglength,streamingwidth, bw)).io)

    // it will take paddinglength/streamingwidth cycles to finalize one iteration
    val ccs = paddinglength/streamingwidth // 96/2 = 48 cycles for output stage

    val counter = new Counter(ccs)
    val out_valid_reg = RegInit(false.B)
    val out_strt_cnt_reg = RegInit(true.B) // default is set to true
    val vld_rgs = RegInit(VecInit.fill(ccs)(false.B))

    io.out_valid := out_valid_reg
    val out_save = RegInit(VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw))))
    when(io.in_en){
      io.out_en := out_strt_cnt_reg
    }.otherwise{
      io.out_en := false.B
    }

    when(io.in_en && io.in_valid){
      when(counter.value === (ccs - 1).U) { // controls switching buffer modes and tracking stage
        out_strt_cnt_reg := true.B
        when(switchModes){
          SRAM_Buffers(0).in_rst := true.B
          SRAM_Buffers(1).in_rst := false.B
        }.otherwise{
          SRAM_Buffers(0).in_rst := false.B
          SRAM_Buffers(1).in_rst := true.B
        }
        switchModes := ~switchModes
        when(cnt_stg === (segment_lengths.length - 1).U) {
          cnt_stg := 0.U
        }.otherwise {
          cnt_stg := cnt_stg + 1.U
        }
      }.otherwise {
        when(cnt_ccs === ccs_seg.out_data - 1.U) {
          out_strt_cnt_reg := false.B
        }.elsewhen(cnt_ccs < ccs_seg.out_data) {
          out_strt_cnt_reg := true.B
        }
        SRAM_Buffers(0).in_rst := false.B
        SRAM_Buffers(1).in_rst := false.B
        switchModes := switchModes
        cnt_stg := cnt_stg
      }

      sind_seg.in_addr := cnt_stg // take stage as input
      ccs_seg.in_addr := cnt_stg
      offs_seg.in_addr := cnt_stg
      SRAM_Buffers(0).in_offset := offs_seg.out_data
      SRAM_Buffers(1).in_offset := offs_seg.out_data

      when(cnt_ccs < ccs_seg.out_data){
        cnt_ccs := cnt_ccs + 1.U
        SRAM_Buffers(0).in_en := true.B
        SRAM_Buffers(1).in_en := true.B
      }.otherwise{
        when(switchModes){
          SRAM_Buffers(0).in_en := true.B
          SRAM_Buffers(1).in_en := false.B
        }.otherwise{
          SRAM_Buffers(0).in_en := false.B
          SRAM_Buffers(1).in_en := true.B
        }
        when(counter.value === (ccs - 1).U){
          cnt_ccs := 0.U
        }.otherwise{
          cnt_ccs := cnt_ccs
        }
      }

      counter.inc()
      SRAM_Buffers(0).in_write_en := ~switchModes
      SRAM_Buffers(1).in_write_en := switchModes
      SRAM_Buffers(0).in_data := io.in
      SRAM_Buffers(1).in_data := io.in
      vld_rgs(0) := io.in_valid
      for (i <- 0 until ccs - 1) {
        vld_rgs(i + 1) := vld_rgs(i)
      }
      out_valid_reg := vld_rgs(ccs-1)
      when(switchModes){
        io.out := SRAM_Buffers(0).out_data
        out_save := SRAM_Buffers(0).out_data
        SRAM_Buffers(0).in_addr := counter.value
        SRAM_Buffers(1).in_addr := (sind_seg.out_data + cnt_ccs)
      }.otherwise{
        io.out := SRAM_Buffers(1).out_data
        out_save := SRAM_Buffers(1).out_data
        SRAM_Buffers(1).in_addr := counter.value
        SRAM_Buffers(0).in_addr := sind_seg.out_data + cnt_ccs
      }
    }.otherwise{
      io.out := out_save
      SRAM_Buffers(0).in_en := false.B // keep
      SRAM_Buffers(1).in_en := false.B
      SRAM_Buffers(0).in_write_en := ~switchModes
      SRAM_Buffers(1).in_write_en := switchModes
      SRAM_Buffers(0).in_data := io.in // changed to keep
      SRAM_Buffers(1).in_data := io.in
      when(switchModes){
        SRAM_Buffers(0).in_addr := counter.value
        SRAM_Buffers(1).in_addr := (sind_seg.out_data + cnt_ccs)
      }.otherwise{
        SRAM_Buffers(1).in_addr := counter.value
        SRAM_Buffers(0).in_addr := sind_seg.out_data + cnt_ccs
      }
      SRAM_Buffers(0).in_offset := offs_seg.out_data
      SRAM_Buffers(1).in_offset := offs_seg.out_data
      SRAM_Buffers(0).in_rst := false.B
      SRAM_Buffers(1).in_rst := false.B
      switchModes := switchModes // make sure switchmodes does not change
      sind_seg.in_addr := cnt_stg // take stage as input
      ccs_seg.in_addr := cnt_stg
      offs_seg.in_addr := cnt_stg
      cnt_stg := cnt_stg
      cnt_ccs := cnt_ccs
    }
  }

  def main(args: Array[String]): Unit = {
//    test(new counter_test(12)){c=>
//      c.io.in_inc_en.poke(true.B)
//      for(i <- 0 until 28){
//        c.clock.step(1)
//        println(s"counter value: ${c.io.out_count.peek().litValue}")
//      }
//    }
//    val pw = new PrintWriter("one_dimensional_padding_streaming.v")
//    pw.println(getVerilogString(new one_dimensional_padding_streaming(96,2,Array(16,34,42,40,18,36,22),32)))
//    pw.close()
    val entries_test = 12
    val streaming_width = 2
    //need to clear out the srams after each use!!!!
//    test(new one_dimensional_padding_streaming(entries_test,streaming_width,Array(4,4,4,4,4,2,2,4,4,2,2,2),32)){c=>
//      c.io.in_en.poke(true.B)
//      for(i <- 0 until (entries_test/streaming_width) * 32){
//        for(j <- 0 until streaming_width){
//          println(((i*streaming_width + j)%(entries_test)+1))
//          c.io.in(j).Re.poke(((i*streaming_width + j)%(entries_test)+1).U)
//          c.io.in(j).Im.poke((i*streaming_width + j+1).U)
//        }
////        println(s"before clock: ${c.io.out_ready.peek().litValue}")
////        println(s"ccbefore: ${c.io.out_cnt.peek().litValue}")
////        println(s"sw_before: ${c.io.out_test.peek().litValue}")
//        println(s"Clock Cycle: ${(i)%(entries_test/streaming_width)}")
//        c.clock.step(1)
////        println(s"sw_after: ${c.io.out_test.peek().litValue}")
////        println(s"ccafter: ${c.io.out_cnt.peek().litValue}")
////        println(s"after clock: ${c.io.out_ready.peek().litValue}")
//        for (j <- 0 until streaming_width) {
//          println(s"Output${j}: ${c.io.out(j).Re.peek().litValue}")
//        }
//      }
//    }
//    println(s"----------------------------------------------------------")
//    println("\n\n\n\n\n")
//    test(new SRAM_Block_streaming(entries_test,streaming_width,32)){c=>
//      c.io.in_en.poke(true.B)
//      c.io.in_write_en.poke(true.B)
//      c.io.in_offset.poke(1.U)
////      c.io.in_addr.poke(0.U)
////      c.io.in_data(0).Re.poke(1.U)
////      c.io.in_data(0).Im.poke(2.U)
////      c.io.in_data(1).Re.poke(3.U)
////      c.io.in_data(1).Im.poke(4.U)
////      c.clock.step(1)
////      c.io.in_write_en.poke(false.B)
////      c.clock.step(1)
////      println(s"output: ${c.io.out_data(0).Re.peek().litValue}")
////      println(s"output: ${c.io.out_data(0).Im.peek().litValue}")
////      println(s"output: ${c.io.out_data(1).Re.peek().litValue}")
////      println(s"output: ${c.io.out_data(1).Im.peek().litValue}")
//      for(i <- 0 until entries_test/streaming_width){
//        c.io.in_addr.poke(i.U)
//        for(j <- 0 until streaming_width){
//          c.io.in_data(j).Re.poke((i*streaming_width + j + 1).U)
//          c.io.in_data(j).Im.poke((i*streaming_width + j).U)
//        }
//        c.clock.step(1)
//      }
//      c.io.in_write_en.poke(false.B)
//      for(i <- 0 until entries_test/streaming_width){
//        c.io.in_addr.poke(i.U)
//        c.clock.step(1)
//        println(s"clock cycle: ${i+1}")
//        for(j <- 0 until streaming_width){
//          println(s"Output${i*streaming_width + j}: ${c.io.out_data(j).Re.peek().litValue}")
//        }
//      }
//    }
//    test(new SRAM_Block(10, 32)) { c =>
//      c.io.in_en.poke(true.B)
//      c.io.in_write_en.poke(true.B)
//      for(i <- 0 until 10){
//        c.io.in_addr.poke(i.U)
//        c.io.in_data.poke((i+1).U)
//        c.clock.step(1)
//      }
//      c.io.in_write_en.poke(false.B)
//      for(i <- 0 until 10){
//        c.io.in_addr.poke(i.U)
//        c.clock.step(1)
//        println(s"Output: ${c.io.out_data.peek().litValue}")
//      }
//    }
    println(s"----------------------------------------------------------")
//    test(new first_padding((for(i <- 1 to 32)yield{i}).toArray,Array(2,2,2,4,4,2,2,4,4,2,2,2),entries_test,streaming_width,32)){c=>
//      c.io.in_en.poke(true.B)
//      for(i <- 0 until 30){
//        for(j <- 0 until entries_test/streaming_width / 2){
//          println(s"clock cycle: ${j + i*entries_test/streaming_width}")
//          c.clock.step(1)
//          println(s"out_valid : ${c.io.out_valid.peek().litValue}")
//          for(k <- 0 until streaming_width){
//            println(s"output(${k}): ${c.io.out_data(k).Re.peek().litValue}")
//          }
//        }
//        println("-=-=-=-=-=-=-=-==-=-=-start of pause-=-=-=-==-=-=-==-=-")
//        c.io.in_en.poke(false.B)
//        for(j <- 0 until entries_test/streaming_width / 2){
//          println(s"clock cycle: ${j + i * entries_test / streaming_width + (entries_test / streaming_width / 2)*1}")
//          c.clock.step(1)
//        }
//        println("-=-=-=-=-=-=-=-end of pause-=-=-=-=-=-=-=-=-=-=-")
//        c.io.in_en.poke(true.B)
//        for (j <- 0 until entries_test / streaming_width / 2) {
//          println(s"clock cycle: ${j + i * entries_test / streaming_width + (entries_test / streaming_width / 2)*2}")
//          c.clock.step(1)
//          println(s"out_valid : ${c.io.out_valid.peek().litValue}")
//          for (k <- 0 until streaming_width) {
//            println(s"output(${k}): ${c.io.out_data(k).Re.peek().litValue}")
//          }
//        }
//      }
//    }
    println(s"----------------------------------------------------------")
//    test(new transposing_buffers_SRAM(144,12,12,2,10)) { c =>
//      for (l <- 0 until 2) {
//        c.io.in_en.poke(true.B)
//        c.io.in_wren.poke(true.B)
//        for (i <- 0 until 12) {
//          for (j <- 0 until 12 by 2) {
//            println((j * 12 + i) / 2)
//            println(((j + 1) * 12 + i) / 2)
//            c.io.in_addr(0).poke((j * 12 + i).U)
//            c.io.in_addr(1).poke(((j + 1) * 12 + i).U)
//            c.io.in_data(0).Re.poke((j * 12 + i).U)
//            c.io.in_data(0).Im.poke(0.U)
//            c.io.in_data(1).Re.poke(((j + 1) * 12 + i).U)
//            c.io.in_data(1).Im.poke(0.U)
//            println(s"Clock Cycle Step: ${i * 6 + j / 2}")
//            c.clock.step(1)
//            println(s"Output(0): ${c.io.out_data(0).Re.peek().litValue}")
//            println(s"Output(1): ${c.io.out_data(1).Re.peek().litValue}")
//          }
//        }
//        c.io.in_wren.poke(false.B)
//        //manual test
//        println("-------")
//        for (i <- 0 until 144 by 2) {
//          println(s"Clock Cycle: ${i / 2}")
//          c.io.in_addr(0).poke(i.U)
//          c.io.in_addr(1).poke((i + 1).U)
//          c.clock.step(1)
//          println(s"Output(0): ${c.io.out_data(0).Re.peek().litValue}")
//          println(s"Output(1): ${c.io.out_data(1).Re.peek().litValue}")
//        }
//        println("next----------------")
//      }
//    }
    println(s"----------------------------------------------------------")
//    // functionality is good now // right now only compatible with streaming width of 2
//    // but the other paramaters u can adjust
//    val elmnts_slice = 12
//    val paddinglength = 12
//    val strwdth = 2
//    val bw = 32
//    test(new transposing_stage(elmnts_slice,paddinglength,strwdth,bw)){ c=>
//      c.io.in_en.poke(true.B)
//      c.io.in_valid.poke(true.B)
//      for(l <- 0 until 1) { // it works
//        for (i <- 0 until elmnts_slice*paddinglength/2) {
//          for (j <- 0 until strwdth) {
//            c.io.in_data(j).Re.poke((i * strwdth + j).U)
//            c.io.in_data(j).Im.poke(0.U)
//          }
//          c.clock.step(1)
//          println(s"valid: ${c.io.out_valid.peek().litValue}")
//        }
//        println("end-----?/--//-??????_/----")
//        for (i <- 0 until elmnts_slice*paddinglength/2) {
//          println(s"valid: ${c.io.out_valid.peek().litValue}")
//
//          for (j <- 0 until strwdth) {
//            println(s"Output(${j}): ${c.io.out_data(j).Re.peek().litValue}")
//          }
//          c.clock.step(1)
//        }
//        println("next---------------")
//      }
//    }
    println("-----------------")
    // now testing the delays of the custom ROM
//    test(new custom_RAM(5,2,4)){c=>
//      c.io.in_en.poke(true.B)
//      c.io.in_wren.poke(true.B)
//      for(i <- 0 until 5){
//        c.io.in_addr.poke(i.U)
//        c.io.in_data.Re.poke((i+1).U)
//        c.io.in_data.Im.poke(0.U)
//        c.clock.step(1)
//        println(s"output: ${c.io.out_data.Re.peek().litValue}")
//      }
//      println()
//      c.io.in_wren.poke(false.B)
//      for(i <- 0 until 5){
//        c.io.in_addr.poke(i.U)
//        if (i > 1 && i < 3)
//          c.io.in_en.poke(false.B)
//        else
//          c.io.in_en.poke(true.B)
//        c.clock.step(1)
//        println(s"output: ${c.io.out_data.Re.peek().litValue}")
//      }
//      println()
//      c.io.in_en.poke(false.B)
//      for(i <- 0 until 5){
//        c.io.in_addr.poke(i.U)
//        c.clock.step(1)
//        println(s"output: ${c.io.out_data.Re.peek().litValue}")
//      }
//      println()
//    }
//    test(new padding_hardware((for(i <- 1 to 32)yield{i}).toArray, Array(12,48), Array(Array(2,2,2,4,4,2,2,4,4,2,2,2),Array(2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2,2,4,4,2),Array(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)),12, 2, 6)){c=>
//      c.io.in_en.poke(true.B)
//      c.clock.setTimeout(0)
//      for(i <- 0 until 250){
//        println(s"Clock Cycle: ${i}")
//        c.clock.step(1)
//        println(s"output valid: ${c.io.out_valid.peek().litValue}")
//        for(j <- 0 until streaming_width){
//          println(s"Output data: ${c.io.out_data(j).Re.peek().litValue}")
//        }
//      }
//    }

    val pw3 = new PrintWriter("FFT_SingleRadix_Streaming_NRO_v2.v")
    pw3.println(getVerilogString(new FFT_SingleRadix_Streaming_NRO_v2(4,2,2,32)))
    pw3.close()

    val pw4 = new PrintWriter("DFT_Symmetric_NRV_v2.v")
    pw4.println(getVerilogString(new DFT_Symmetric_NRV_v2(3,32)))
    pw4.close()

    val pw5 = new PrintWriter("PermutationsWithStreaming_v2.v")
    pw5.println(getVerilogString(new PermutationsWithStreaming_v2(12,3,3,2,0,32)))
    pw5.close()

//    val pw6 = new PrintWriter("PermutationsWithStreaming_mr_v2.v")
//    pw6.println(getVerilogString(new PermutationsWithStreaming_mr_v2(12,4,4, 3,2,0,32,2)))
//    pw6.close()

    val pw6 = new PrintWriter("PermutationsWithStreaming_mr_v2.v")
    pw6.println(getVerilogString(new PermutationsWithStreaming_mr_v2(12, 3, 3, 2, 3, 0, 32, 2)))
    pw6.close()

    val pw7 = new PrintWriter("TwiddleFactorsStreamed_mr_v2.v")
    pw7.println(getVerilogString(new TwiddleFactorsStreamed_mr_v2(12,4,3,3,32,6)))
    pw7.close()

    val pw2 = new PrintWriter("FFT_MixedRadix_Streaming_v2.v")
    pw2.println(getVerilogString(new FFT_MixedRadix_Streaming_v2(12,4,3,2,3,2,32)))
    pw2.close()

    val pw1 = new PrintWriter("spherical_mem_ROM.v")
    pw1.println(getVerilogString(new spherical_mem_ROM((for(i <- 1 to 32)yield{i}).toArray,2,32)))
    pw1.close()

    val pw = new PrintWriter("padding_3D_FFT.v")
    pw.println(getVerilogString(new padding_3D_FFT((for (i <- 1 to 32) yield {
      i
    }).toArray, Array(12, 48), Array(Array(2, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 2), Array(2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2), Array(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)), 12, 2, 32)))
    pw.close()

    test(new padding_3D_FFT((for (i <- 1 to 32) yield {
      i
    }).toArray, Array(12, 48), Array(Array(2, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 2), Array(2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2, 2, 4, 4, 2), Array(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)), 12, 2, 32)) { c =>
      c.io.in_en.poke(true.B)
      c.clock.setTimeout(0)
      for (i <- 0 until 75) {
        println(s"Clock Cycle: ${i}")
        c.clock.step(1)
        println(s"output valid: ${c.io.out_valid.peek().litValue}")
        for (j <- 0 until streaming_width) {
          println(s"Real Output: ${convert_long_to_float(c.io.out_data(j).Re.peek().litValue, 32)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out_data(j).Im.peek().litValue, 32)}")
        }
      }
//      c.io.in_en.poke(false.B)
//      for(i <- 0 until 75){
//        c.clock.step(1)
//      }
      c.io.in_en.poke(true.B)
      for (i <- 0 until 1000) {
        println(s"Clock Cycle: ${i + 75}")
        c.clock.step(1)
        println(s"output valid: ${c.io.out_valid.peek().litValue}")
        for (j <- 0 until streaming_width) {
          println(s"Real Output: ${convert_long_to_float(c.io.out_data(j).Re.peek().litValue,32)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out_data(j).Im.peek().litValue, 32)}")
        }
      }
    }
    println("-----------------")
//    test(new transposing_buffers_SRAM_v2(144,12,12,2,32)) { c =>
//      for (l <- 0 until 3) {
//        c.io.in_en.poke(true.B)
//        c.io.in_wren.poke(true.B)
//        for (i <- 0 until 144 / 2) {
//          println(s"Clock Cycle: ${i}")
//          for (j <- 0 until 2) {
//            c.io.in_data(j).Re.poke((i * 2 + j).U)
//            c.io.in_data(j).Im.poke(0.U)
//          }
//          c.clock.step(1)
//        }
//        c.io.in_wren.poke(false.B)
//        for (i <- 0 until 144 / 2) {
//          println(s"Clock Cycle: ${i + 144 / 2}")
//          for (j <- 0 until 2) {
//            println(s"Output($j): ${c.io.out_data(j).Re.peek().litValue}")
//          }
//          c.clock.step(1)
//        }
//      }
//    }
  }

  // intialize a ROM for constants
  class constants_ROM(constants: Array[Int], maxSize: Int) extends Module {
    val io = IO(new Bundle{
      val in_addr = Input(UInt(log2Ceil(constants.length).W))
      val out_data = Output(UInt(log2Ceil(maxSize).W))
    })
    val offsets_Vec = VecInit(constants.map(x=>x.U))
    io.out_data := offsets_Vec(io.in_addr)
  }

  class spherical_mem_ROM(sphere: Array[Int], streaming_width: Int, bw: Int) extends Module{
    val io = IO(new Bundle{
      val in_en = Input(Bool())
      val out_data = Output(Vec(streaming_width, new ComplexNum(bw)))
      val out_valid = Output(Bool())
    })
    val sphere_ROM = RegInit(VecInit(sphere.map(x=>(convert_string_to_IEEE_754(x.toDouble.toString, bw).U(bw.W)))))
    val counter = new Counter(sphere.length/streaming_width)
    for(i <- 0 until streaming_width){
      io.out_data(i).Re := sphere_ROM(counter.value*streaming_width.U + i.U)
      io.out_data(i).Im := 0.U
    }
    val out_valid_reg = RegInit(false.B)
    when(io.in_en){ // already good
      counter.inc()
      io.out_valid := true.B
      out_valid_reg := true.B
    }.otherwise{
      io.out_valid := out_valid_reg
    }
  }
  // this version does not work properly
  class transposing_buffers_SRAM(entries: Int, elements_per_slice: Int,paddingLength: Int, streaming_width: Int,bw: Int) extends Module{
    val io = IO(new Bundle{
      val in_en = Input(Bool())
      val in_data = Input(Vec(streaming_width, new ComplexNum(bw)))
      val in_addr = Input(Vec(streaming_width, UInt(log2Ceil(entries).W)))
      val in_wren = Input(Bool())
      val out_data = Output(Vec(streaming_width, new ComplexNum(bw)))
    })
    val number_of_elements_per_SRAM = entries/streaming_width
    val write_alternate = new Counter(2)
    val write_alternate_secondary = new Counter(paddingLength/streaming_width)
    val read_alternate = new Counter(elements_per_slice/streaming_width)
    val read_alternate_flag = RegInit(false.B)
    val read_alternate_flag_delay = RegInit(false.B)
    read_alternate_flag := read_alternate_flag_delay // very strange behavior with this module for some reason
    val new_rams = VecInit.fill(streaming_width)(Module(new custom_RAM(number_of_elements_per_SRAM, streaming_width,bw)).io)
    val out_save = VecInit.fill(streaming_width)(0.U.asTypeOf(new ComplexNum(bw)))
    new_rams(0).in_wren := io.in_wren
    new_rams(1).in_wren := io.in_wren
    new_rams(0).in_en := io.in_en
    new_rams(1).in_en := io.in_en
    when(io.in_en){
      when(io.in_wren){
        when(write_alternate.value === 1.U){
          new_rams(1).in_addr := io.in_addr(0) >> 1.U
          new_rams(0).in_addr := io.in_addr(1) >> 1.U
          new_rams(1).in_data := io.in_data(0)
          new_rams(0).in_data := io.in_data(1)
        }.otherwise{
          new_rams(1).in_addr := io.in_addr(1) >> 1.U
          new_rams(0).in_addr := io.in_addr(0) >> 1.U
          new_rams(1).in_data := io.in_data(1)
          new_rams(0).in_data := io.in_data(0)
        }
        when(write_alternate_secondary.value === (paddingLength/streaming_width - 1).U){
          write_alternate.inc()
        }
        write_alternate_secondary.inc()
        io.out_data := out_save
      }.otherwise{
        when(read_alternate_flag){
          new_rams(1).in_addr := io.in_addr(0) >> 1.U
          new_rams(0).in_addr := io.in_addr(1) >> 1.U
          new_rams(1).in_data := io.in_data(0)
          new_rams(0).in_data := io.in_data(1)
          io.out_data(0) := new_rams(1).out_data
          io.out_data(1) := new_rams(0).out_data
          out_save(0) := new_rams(1).out_data
          out_save(1) := new_rams(0).out_data
        }.otherwise{
          new_rams(1).in_addr := io.in_addr(1) >> 1.U
          new_rams(0).in_addr := io.in_addr(0) >> 1.U
          new_rams(1).in_data := io.in_data(1)
          new_rams(0).in_data := io.in_data(0)
          io.out_data(0) := new_rams(0).out_data
          io.out_data(1) := new_rams(1).out_data
          out_save(0) := new_rams(0).out_data
          out_save(1) := new_rams(1).out_data
        }
        read_alternate.inc()
        when(read_alternate.value === ((elements_per_slice/streaming_width) - 1).U){
          read_alternate_flag_delay := ~read_alternate_flag_delay
        }
      }
    }.otherwise{
      when(io.in_wren) {
        when(write_alternate.value === 1.U) {
          new_rams(1).in_addr := io.in_addr(0) >> 1.U
          new_rams(0).in_addr := io.in_addr(1) >> 1.U
          new_rams(1).in_data := io.in_data(0)
          new_rams(0).in_data := io.in_data(1)
        }.otherwise {
          new_rams(1).in_addr := io.in_addr(1) >> 1.U
          new_rams(0).in_addr := io.in_addr(0) >> 1.U
          new_rams(1).in_data := io.in_data(1)
          new_rams(0).in_data := io.in_data(0)
        }
        io.out_data := out_save
      }.otherwise {
        when(read_alternate_flag) {
          new_rams(1).in_addr := io.in_addr(0) >> 1.U
          new_rams(0).in_addr := io.in_addr(1) >> 1.U
          new_rams(1).in_data := io.in_data(0)
          new_rams(0).in_data := io.in_data(1)
          io.out_data(0) := new_rams(1).out_data
          io.out_data(1) := new_rams(0).out_data
        }.otherwise {
          new_rams(1).in_addr := io.in_addr(1) >> 1.U
          new_rams(0).in_addr := io.in_addr(0) >> 1.U
          new_rams(1).in_data := io.in_data(1)
          new_rams(0).in_data := io.in_data(0)
          io.out_data(0) := new_rams(0).out_data
          io.out_data(1) := new_rams(1).out_data
        }
      }
    }
  }


  class transposing_buffers_SRAM_v2(entries: Int, elements_per_slice: Int, paddingLength: Int, streaming_width: Int, bw: Int) extends Module {
    val io = IO(new Bundle {
      val in_en = Input(Bool())
      val in_data = Input(Vec(streaming_width, new ComplexNum(bw)))
      val in_valid = Input(Bool())
      val in_wren = Input(Bool())
      val out_data = Output(Vec(streaming_width, new ComplexNum(bw)))
    })
    val number_of_elements_per_SRAM = (entries / streaming_width) / streaming_width
    val mem_side_select_global = new Counter(streaming_width)
    val mem_side_select_local = new Counter(streaming_width) // primarily used for the reading stage
    val offset_counter_store = new Counter(number_of_elements_per_SRAM/(paddingLength/streaming_width))
    val offset_counter_read = new Counter(number_of_elements_per_SRAM/(paddingLength/streaming_width))
    val offset_amnt = paddingLength/streaming_width // used as offset
    val padding_store_counter_addr = new Counter(paddingLength/streaming_width)
    val padding_read_counter_addr = new Counter(paddingLength/streaming_width)
    val out_temp_reg = RegInit(VecInit.fill(streaming_width)(0.U.asTypeOf(new ComplexNum(bw)))) // will temp hold the prev output
    val buffs = VecInit.fill(streaming_width*streaming_width)(Module(new custom_RAM(number_of_elements_per_SRAM, streaming_width, bw)).io)
    for(i <- 0 until streaming_width * streaming_width){
      buffs(i).in_wren := io.in_wren
    }
    for (i <- 0 until streaming_width * streaming_width) {
      buffs(i).in_data := io.in_data(i % streaming_width)
    }

    when(io.in_en & io.in_valid){
      when(io.in_wren){

        for (i <- 0 until streaming_width * streaming_width) {
          buffs(i).in_addr := padding_store_counter_addr.value + (offset_counter_store.value * offset_amnt.U)
        }

        padding_store_counter_addr.inc()

        when(padding_store_counter_addr.value === ((paddingLength/streaming_width) - 1).U && mem_side_select_global.value === (streaming_width - 1).U){
          offset_counter_store.inc()
          mem_side_select_global.inc()
        }.elsewhen(padding_store_counter_addr.value === ((paddingLength/streaming_width) - 1).U){
          mem_side_select_global.inc()
        }

        when(mem_side_select_global.value === 1.U){
          for(i <- 0 until (streaming_width * streaming_width) / streaming_width) {
            buffs(i).in_en := false.B
            buffs(i+streaming_width).in_en := true.B
          }
        }.otherwise{
          for (i <- 0 until (streaming_width * streaming_width) / streaming_width) {
            buffs(i).in_en := true.B
            buffs(i + streaming_width).in_en := false.B
          }
        }

        io.out_data := out_temp_reg

      }.otherwise{

        for (i <- 0 until streaming_width * streaming_width) {
          buffs(i).in_addr := padding_read_counter_addr.value + (offset_counter_read.value * offset_amnt.U)
        }

        offset_counter_read.inc()

        when(offset_counter_read.value === ((number_of_elements_per_SRAM/(paddingLength/streaming_width)) - 1).U && mem_side_select_local.value === (streaming_width - 1).U){
          padding_read_counter_addr.inc()
          mem_side_select_local.inc()
        }.elsewhen(offset_counter_read.value === ((number_of_elements_per_SRAM/(paddingLength/streaming_width)) - 1).U){
          mem_side_select_local.inc()
        }

        when(mem_side_select_local.value === 1.U){
          for (i <- 0 until (streaming_width * streaming_width)) {
            if (i%streaming_width == 1) {
              buffs(i).in_en := true.B
            } else {
              buffs(i).in_en := false.B
            }
          }
          io.out_data(0) := buffs(1).out_data
          io.out_data(1) := buffs(3).out_data
          out_temp_reg(0) := buffs(1).out_data
          out_temp_reg(1) := buffs(3).out_data
        }.otherwise{
          for (i <- 0 until (streaming_width * streaming_width)) {
            if (i % streaming_width == 0) {
              buffs(i).in_en := true.B
            } else {
              buffs(i).in_en := false.B
            }
          }
          io.out_data(0) := buffs(0).out_data
          io.out_data(1) := buffs(2).out_data
          out_temp_reg(0) := buffs(0).out_data
          out_temp_reg(1) := buffs(2).out_data
        }

      }
    }.otherwise{
      for(i <- 0 until streaming_width * streaming_width){
        buffs(i).in_en := false.B
      }
      when(io.in_wren){
        for (i <- 0 until streaming_width * streaming_width) {
          buffs(i).in_addr := padding_store_counter_addr.value + (offset_counter_store.value * offset_amnt.U)
        }
      }.otherwise{
        for (i <- 0 until streaming_width * streaming_width) {
          buffs(i).in_addr := padding_read_counter_addr.value + (offset_counter_read.value * offset_amnt.U)
        }
      }
      io.out_data := out_temp_reg

    }
  }

  // this version does not work
  class transposing_stage(elmnts_slice: Int, paddinglength: Int, streaming_width: Int, bw: Int) extends Module{
    val io = IO(new Bundle{
      val in_data = Input(Vec(streaming_width, new ComplexNum(bw)))
      val in_en = Input(Bool())
      val in_valid = Input(Bool())
      val out_valid = Output(Bool())
      val out_start_cnt = Output(Bool())
      val out_data = Output(Vec(streaming_width, new ComplexNum(bw)))
    })
    val buffers = VecInit.fill(2)(Module(new transposing_buffers_SRAM(elmnts_slice*paddinglength, elmnts_slice,paddinglength, streaming_width, bw)).io)
    val slice_counter = new Counter(paddinglength/2)
    val elmnt_per_slice_counter = new Counter(elmnts_slice)
    val out_counter = new Counter(elmnts_slice * paddinglength / 2)
    val switchmodes = RegInit(false.B)
    val in_address_calc = WireInit(VecInit.fill(streaming_width)(0.U(log2Ceil(elmnts_slice*paddinglength).W)))
    val out_valid_regs = RegInit(VecInit.fill(elmnts_slice*paddinglength/2)(false.B))
    val out_strt_cnt_reg = RegInit(true.B)
    val out_address_calc = WireInit(VecInit.fill(streaming_width)(0.U(log2Ceil(elmnts_slice*paddinglength).W)))
    buffers(0).in_wren := ~switchmodes // is in write mode inititally
    buffers(1).in_wren := switchmodes // is in read mode initially because the switchmode reg is initialized to 0.U
    in_address_calc(0) := (slice_counter.value * elmnts_slice.U) +& (slice_counter.value * elmnts_slice.U) +& elmnt_per_slice_counter.value // kind of like an offset
    in_address_calc(1) := (slice_counter.value * elmnts_slice.U) +& (slice_counter.value * elmnts_slice.U) +& elmnts_slice.U +& elmnt_per_slice_counter.value
    for (i <- 0 until streaming_width) {
      out_address_calc(i) := (out_counter.value * streaming_width.U) + i.U
    }
    buffers(0).in_en := io.in_en
    buffers(1).in_en := io.in_en
    val ovreg = RegInit(false.B)
    io.out_valid := out_valid_regs((elmnts_slice*paddinglength/2) -1)
    buffers(0).in_data := io.in_data
    buffers(1).in_data := io.in_data
    when(io.in_en){
      io.out_start_cnt := out_strt_cnt_reg
      out_strt_cnt_reg := true.B
    }.otherwise{
      io.out_start_cnt := false.B
    }
    when(io.in_en && io.in_valid){
      slice_counter.inc() // slice counter counts for 6 cycles before incrementing the elmnt_per_slice_couner
      when(slice_counter.value === (paddinglength/2 - 1).U){
        elmnt_per_slice_counter.inc()
      }
      out_counter.inc()
      when(out_counter.value === ((elmnts_slice*paddinglength / 2) - 1).U){
        switchmodes := ~switchmodes
      }
      when(switchmodes){ // buf 0 is in read mode and buf 1 is in write mode
        for(i <- 0 until streaming_width){
          buffers(0).in_addr(i) := out_address_calc(i)
          buffers(1).in_addr(i) := in_address_calc(i)
        }
        io.out_data := buffers(0).out_data
      }.otherwise{ // buf 0 is in write mode and buf 1 is in read mode
        for(i <- 0 until streaming_width){
          buffers(0).in_addr(i) := in_address_calc(i)
          buffers(1).in_addr(i) := out_address_calc(i)
        }
        io.out_data := buffers(1).out_data
      }
      out_valid_regs(0) := io.in_valid
      for(i <- 0 until (elmnts_slice*paddinglength/2)-1){
        out_valid_regs(i+1) := out_valid_regs(i)
      }
      ovreg := out_valid_regs((elmnts_slice*paddinglength/2) -1)
    }.otherwise{
      switchmodes := switchmodes // no change
      when(switchmodes){
        for(i <- 0 until streaming_width){
          buffers(0).in_addr(i) := out_address_calc(i)
          buffers(1).in_addr(i) := in_address_calc(i)
        }
        io.out_data := buffers(0).out_data
      }.otherwise{
        for(i <- 0 until streaming_width){
          buffers(0).in_addr(i) := in_address_calc(i)
          buffers(1).in_addr(i) := out_address_calc(i)
          io.out_data := buffers(1).out_data
        }
      }
    }
  }

  class transposing_stage_v2(elmnts_slice: Int, paddinglength: Int, streaming_width: Int, bw: Int) extends Module {
    val io = IO(new Bundle {
      val in_data = Input(Vec(streaming_width, new ComplexNum(bw)))
      val in_en = Input(Bool())
      val in_valid = Input(Bool())
      val out_valid = Output(Bool())
      val out_en = Output(Bool())
      val out_data = Output(Vec(streaming_width, new ComplexNum(bw)))
    })
    val buffers = VecInit.fill(2)(Module(new transposing_buffers_SRAM_v2(elmnts_slice * paddinglength, elmnts_slice, paddinglength, streaming_width, bw)).io)
    val out_counter = new Counter(elmnts_slice * paddinglength / 2)
    val switchmodes = RegInit(false.B)
    val out_valid_regs = RegInit(VecInit.fill(elmnts_slice * paddinglength / 2)(false.B))
    val out_strt_cnt_reg = RegInit(true.B)

    buffers(0).in_wren := ~switchmodes
    buffers(1).in_wren := switchmodes

    buffers(0).in_en := io.in_en
    buffers(1).in_en := io.in_en
    buffers(0).in_valid := io.in_valid
    buffers(1).in_valid := io.in_valid
    val ovreg = RegInit(false.B)
    io.out_valid := out_valid_regs((elmnts_slice * paddinglength / 2) - 1)
    buffers(0).in_data := io.in_data
    buffers(1).in_data := io.in_data
    when(io.in_en) {
      io.out_en := out_strt_cnt_reg
      out_strt_cnt_reg := true.B
    }.otherwise {
      io.out_en := false.B
    }
    when(io.in_en && io.in_valid) {
      out_counter.inc()
      when(out_counter.value === ((elmnts_slice * paddinglength / 2) - 1).U) {
        switchmodes := ~switchmodes
      }
      when(switchmodes) { // buf 0 is in read mode and buf 1 is in write mode
        io.out_data := buffers(0).out_data
      }.otherwise { // buf 0 is in write mode and buf 1 is in read mode
        io.out_data := buffers(1).out_data
      }
      out_valid_regs(0) := io.in_valid
      for (i <- 0 until (elmnts_slice * paddinglength / 2) - 1) {
        out_valid_regs(i + 1) := out_valid_regs(i)
      }
      ovreg := out_valid_regs((elmnts_slice * paddinglength / 2) - 1)
    }.otherwise {
      when(switchmodes) {
        io.out_data := buffers(0).out_data
      }.otherwise {
        io.out_data := buffers(1).out_data
      }
    }
  }


  class first_padding(sphere: Array[Int], segment_lengths: Array[Int], paddinglength: Int, streaming_width: Int,bw: Int)extends Module{
    val io = IO(new Bundle{
      val in_en = Input(Bool())
      val out_data = Output(Vec(2, new ComplexNum(bw)))
      val out_valid = Output(Bool())
    })
    val sphrom = Module(new spherical_mem_ROM(sphere, streaming_width, bw)).io
    val odpad = Module(new one_dimensional_padding_streaming(paddinglength, streaming_width, segment_lengths, bw)).io
    sphrom.in_en := odpad.out_en
    odpad.in_valid := sphrom.out_valid
    odpad.in_en := io.in_en
    io.out_valid := odpad.out_valid
    io.out_data := odpad.out
    odpad.in := sphrom.out_data
  }

  class custom_RAM(entries: Int, streaming_width: Int,bw: Int)extends Module{
    val io = IO(new Bundle{
      val in_data = Input(new ComplexNum(bw))
      val in_en = Input(Bool())
      val in_wren = Input(Bool())
      val in_addr = Input(UInt(log2Ceil(entries).W))
      val out_data = Output(new ComplexNum(bw))
    })
    val rams = RegInit(VecInit.fill(entries)(0.U.asTypeOf(new ComplexNum(bw))))
    val reg_out = RegInit(0.U.asTypeOf(new ComplexNum(bw)))
    when(io.in_en) {
      when(io.in_wren) {
        rams(io.in_addr) := io.in_data
        io.out_data := reg_out
      }.otherwise {
        reg_out := rams(io.in_addr)
        io.out_data := rams(io.in_addr)
      }
    }.otherwise{
      io.out_data := reg_out
    }
  }

  // the functionality has been manually verified
  class padding_3D_FFT(sphere: Array[Int], elmnts_slice: Array[Int], segment_lengths: Array[Array[Int]], paddingLength: Int, streaming_width: Int, bw: Int) extends Module {
    val io = IO {
      new Bundle {
        val in_en = Input(Bool())
        val out_data = Output(Vec(streaming_width, new ComplexNum(bw)))
        val out_valid = Output(Bool())
      }
    }
    val sphere_rom = Module(new spherical_mem_ROM(sphere, streaming_width, bw)).io
    val odpad_first_stage = Module(new one_dimensional_padding_streaming(paddingLength, streaming_width, segment_lengths(0), bw)).io
    val odfft_first_stage = Module(new FFT_MixedRadix_Streaming_v2(paddingLength, 4, 3, 2, 3, streaming_width, bw)).io
    val transposing_first_stage = Module(new transposing_stage_v2(elmnts_slice(0), paddingLength, streaming_width, bw)).io
    val odpad_second_stage = Module(new one_dimensional_padding_streaming(paddingLength, streaming_width, segment_lengths(1), bw)).io
    val odfft_second_stage = Module(new FFT_MixedRadix_Streaming_v2(paddingLength, 4, 3, 2, 3, streaming_width, bw)).io
    val transposing_second_stage = Module(new transposing_stage_v2(elmnts_slice(1), paddingLength, streaming_width, bw)).io
    val odpad_third_stage = Module(new one_dimensional_padding_streaming(paddingLength, streaming_width, segment_lengths(2), bw)).io
    val odfft_third_stage = Module(new FFT_MixedRadix_Streaming_v2(paddingLength, 4, 3, 2, 3, streaming_width, bw)).io

    sphere_rom.in_en := odpad_first_stage.out_en

    odpad_first_stage.in_en := odfft_first_stage.out_en
    odpad_first_stage.in_valid := sphere_rom.out_valid
    odpad_first_stage.in := sphere_rom.out_data

    odfft_first_stage.in_en := transposing_first_stage.out_en
    odfft_first_stage.in_ready := odpad_first_stage.out_valid
    odfft_first_stage.in := odpad_first_stage.out
//
    transposing_first_stage.in_data := odfft_first_stage.out
    transposing_first_stage.in_en := odpad_second_stage.out_en
    transposing_first_stage.in_valid := odfft_first_stage.out_validate

    odpad_second_stage.in := transposing_first_stage.out_data
    odpad_second_stage.in_en := odfft_second_stage.out_en
    odpad_second_stage.in_valid := transposing_first_stage.out_valid
//
    odfft_second_stage.in_en := transposing_second_stage.out_en
    odfft_second_stage.in_ready := odpad_second_stage.out_valid
    odfft_second_stage.in := odpad_second_stage.out
//
    transposing_second_stage.in_data := odfft_second_stage.out
    transposing_second_stage.in_en := odpad_third_stage.out_en
    transposing_second_stage.in_valid := odfft_second_stage.out_validate
//
    odpad_third_stage.in := transposing_second_stage.out_data
    odpad_third_stage.in_valid := transposing_second_stage.out_valid //transposing_second_stage.out_valid
    odpad_third_stage.in_en := odfft_third_stage.out_en
//
    odfft_third_stage.in_en := io.in_en
    odfft_third_stage.in_ready := odpad_third_stage.out_valid
    odfft_third_stage.in := odpad_third_stage.out

    io.out_data := odfft_third_stage.out//odfft_third_stage.out //transposing_first_stage.out_data//odpad_first_stage.out //odpad_third_stage.out
    io.out_valid := odfft_third_stage.out_validate//odfft_third_stage.out_validate //transposing_first_stage.out_valid//odpad_first_stage.out_valid //odpad_third_stage.out_valid
  }
}
