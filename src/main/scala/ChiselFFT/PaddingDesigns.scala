package ChiselFFT
import chisel3._
import Chisel.log2Ceil
import ChiselFFT.DFTDesigns.DFT_Symmetric_NRV_v2
import chisel3.util._

import scala.collection.mutable
import ComplexModules.FPComplex._
import IEEEConversions.FPConvert._
import ChiselFFT.FFTMRDesigns.{ FFT_MixedRadix_Streaming_v2}
import ChiselFFT.FFTSRDesigns.{ FFT_SingleRadix_Streaming_NRO_v2}
import ChiselFFT.PermutationDesigns._
import ChiselFFT.TwidFactorDesigns.TwiddleFactorsStreamed_mr_v2
import chisel3.tester._
import chisel3.tester.RawTester.test

import java.io.PrintWriter
import FloatingPointDesigns.FPArithmetic._


object PaddingDesigns {
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
    val reg_out = RegInit(VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw))))
    when(io.in_rst) { // clear the memory if reset (might move to within the enable statement)
      for (i <- 0 until entries / streamingwidth) {
        for (j <- 0 until streamingwidth) {
          sram_mems_2(j)(i) := 0.U.asTypeOf(new ComplexNum(bw))
        }
      }
      io.out_data := VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw)))
      reg_out := VecInit.fill(2)(0.U.asTypeOf(new ComplexNum(bw)))
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
        io.out_data := reg_out
      }.otherwise {
        for (i <- 0 until streamingwidth) {
          io.out_data(i) := reg_out(i)
          reg_out(i) := sram_mems_2(i)(io.in_addr)
        }
      }
    }.otherwise {
      io.out_data := reg_out
    }
  }

  class one_dimensional_padding_streaming(paddinglength: Int, streamingwidth: Int, segment_lengths: Array[Int], bw: Int) extends Module{
    val io = IO(new Bundle{
      val in = Input(Vec(streamingwidth, new ComplexNum(bw)))
      val in_last = Input(Bool())
      val in_ready = Input(Bool())
      val in_valid = Input(Bool())
      val out = Output(Vec(streamingwidth, new ComplexNum(bw)))
      val out_last = Output(Bool())
      val out_ready = Output(Bool())
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
    val switchModes_2 = RegInit(false.B)


    val SRAM_Buffers = for(i <- 0 until 2) yield{
      val mod = Module(new RAM_Block_streaming_v2(paddinglength,streamingwidth,bw)).io
      mod
    }
//    val SRAM_Buffers = VecInit.fill(2)(Module(new RAM_Block_streaming_v2(paddinglength,streamingwidth, bw)).io)

    // it will take paddinglength/streamingwidth cycles to finalize one iteration
    val ccs = paddinglength/streamingwidth // 96/2 = 48 cycles for output stage

    val counter = new Counter(ccs)
    val out_valid_reg = RegInit(false.B)
    val out_last_reg = RegInit(false.B)
    val out_ready_reg = RegInit(true.B) // default is set to true
    val vld_rgs = RegInit(VecInit.fill(ccs)(VecInit.fill(2)(false.B)))

    io.out_valid := out_valid_reg
    io.out_last := out_last_reg
    val out_save = RegInit(VecInit.fill(streamingwidth)(0.U.asTypeOf(new ComplexNum(bw))))
    when(io.in_ready){
      io.out_ready := out_ready_reg
    }.otherwise{
      io.out_ready := false.B
    }

    when(io.in_ready && io.in_valid){
      when(counter.value === (ccs - 1).U) { // controls switching buffer modes and tracking stage
        out_ready_reg := true.B
        when(switchModes){
          SRAM_Buffers(0).in_rst := true.B
          SRAM_Buffers(1).in_rst := false.B
        }.otherwise{
          SRAM_Buffers(0).in_rst := false.B
          SRAM_Buffers(1).in_rst := true.B
        }
        switchModes := ~switchModes
        switchModes_2 := switchModes
        when(cnt_stg === (segment_lengths.length - 1).U) {
          cnt_stg := 0.U
        }.otherwise {
          cnt_stg := cnt_stg + 1.U
        }
      }.otherwise {
        when(cnt_ccs === ccs_seg.out_data - 1.U) {
          out_ready_reg := false.B
        }.elsewhen(cnt_ccs < ccs_seg.out_data) {
          out_ready_reg := true.B
        }
        SRAM_Buffers(0).in_rst := false.B
        SRAM_Buffers(1).in_rst := false.B
        switchModes := switchModes
        switchModes_2 := switchModes
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
      vld_rgs(0)(0) := io.in_valid
      vld_rgs(0)(1) := io.in_last
      for (i <- 0 until ccs - 1) {
        vld_rgs(i + 1) := vld_rgs(i)
      }
      out_valid_reg := vld_rgs(ccs-1)(0)
      out_last_reg := vld_rgs(ccs-1)(1)
      when(switchModes_2){
        io.out := SRAM_Buffers(0).out_data
        out_save := SRAM_Buffers(0).out_data
      }.otherwise{
        io.out := SRAM_Buffers(1).out_data
        out_save := SRAM_Buffers(1).out_data
      }

      when(switchModes){
//        io.out := SRAM_Buffers(0).out_data
//        out_save := SRAM_Buffers(0).out_data
        SRAM_Buffers(0).in_addr := counter.value
        SRAM_Buffers(1).in_addr := (sind_seg.out_data + cnt_ccs)
      }.otherwise{
//        io.out := SRAM_Buffers(1).out_data
//        out_save := SRAM_Buffers(1).out_data
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

  // intialize a ROM for constants
  class constants_ROM(constants: Array[Int], maxSize: Int) extends Module {
    val io = IO(new Bundle{
      val in_addr = Input(UInt(log2Ceil(constants.length).W))
      val out_data = Output(UInt(log2Ceil(maxSize).W))
    })
    val offsets_Vec = VecInit(constants.map(x=>x.U))
    io.out_data := offsets_Vec(io.in_addr)
  }

  def main(args: Array[String]): Unit = {
    val entries_test = 12
    val streaming_width = 2

    //    val pw3 = new PrintWriter("FFT_SingleRadix_Streaming_NRO_v2.v")
    //    pw3.println(getVerilogString(new FFT_SingleRadix_Streaming_NRO_v2(4,2,2,32)))
    //    pw3.close()
    //
    //    val pw4 = new PrintWriter("DFT_Symmetric_NRV_v2.v")
    //    pw4.println(getVerilogString(new DFT_Symmetric_NRV_v2(3,32)))
    //    pw4.close()
    //
    //    val pw5 = new PrintWriter("PermutationsWithStreaming_v2.v")
    //    pw5.println(getVerilogString(new PermutationsWithStreaming_v2(12,3,3,2,0,32)))
    //    pw5.close()
    //
    ////    val pw6 = new PrintWriter("PermutationsWithStreaming_mr_v2.v")
    ////    pw6.println(getVerilogString(new PermutationsWithStreaming_mr_v2(12,4,4, 3,2,0,32,2)))
    ////    pw6.close()
    //
    //    val pw6 = new PrintWriter("PermutationsWithStreaming_mr_v2.v")
    //    pw6.println(getVerilogString(new PermutationsWithStreaming_mr_v2(12, 3, 3, 2, 3, 0, 32, 2)))
    //    pw6.close()
    //
    //    val pw7 = new PrintWriter("TwiddleFactorsStreamed_mr_v2.v")
    //    pw7.println(getVerilogString(new TwiddleFactorsStreamed_mr_v2(12,4,3,3,32,6)))
    //    pw7.close()
    //
    //    val pw2 = new PrintWriter("FFT_MixedRadix_Streaming_v2.v")
    //    pw2.println(getVerilogString(new FFT_MixedRadix_Streaming_v2(12,4,3,2,3,2,32)))
    //    pw2.close()
    //

    println("-----------------")
  }
}
