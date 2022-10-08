package ChiselFFT
import chisel3._
import chisel3.util._
import ComplexModules.FPComplex._
import SWFFT._
import SWFFT.FFT._
import IEEEConversions.FPConvert._
import Chisel.{MuxLookup, log2Ceil, resetToBool}
import ComplexModules.FPComplex
import SWFFT.ComplexNumbers.cmplx
import FFTSRDesigns._
import FFTMRDesigns._
import TwidFactorDesigns._
import DFTDesigns._

import scala.collection.mutable

object PermutationDesigns {
  class PermutationsSimple(N:Int, r: Int, Type: Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in = Input(Vec(N, new ComplexNum(bw)))
      val out = Output(Vec(N, new ComplexNum(bw)))
    })
    val PermutationModel = (if(Type == 1){Permutations.R[Int]((for(i <- 0 until N)yield{i}).toArray, N, r)}else{Permutations.L[Int]((for(i <- 0 until N)yield{i}).toArray, N, r)}).toVector
    for(i <- 0 until N){
      io.out(i) := io.in(PermutationModel(i))
    }
  }

  class Permutations_forIterative(N:Int, r: Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in = Input(Vec(N, new ComplexNum(bw)))
      val in_perm_data = Input(Vec(N, UInt(log2Ceil(N).W)))
      val out = Output(Vec(N, new ComplexNum(bw)))
    })
    val results = RegInit(VecInit.fill(N)(0.U.asTypeOf(new ComplexNum(bw))))
    for(i <- 0 until N){
      results(i) := io.in(io.in_perm_data(i))
    }
    io.out := results
  }

  class PermutationsWithStreaming(N:Int, r: Int, base_r: Int, w:Int, ptype: Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec((N/w)*2 + 1, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val is_enabled = RegInit(false.B)
    val offset = (N/w).U
    val offset_switch = RegInit(0.U(1.W))
    when(io.in_en.asUInt.orR){
      is_enabled := true.B
    }.otherwise{
      is_enabled := false.B
    }
    val M0 = (for(i <- 0 until w) yield{
      val M0_t = Module(new RAM_Block(N,w,bw)).io
      M0_t
    }).toVector
    val M1 = (for(i <- 0 until w) yield{
      val M1_t = Module(new RAM_Block(N,w,bw)).io
      M1_t
    }).toVector
    val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io
    val M0_config = Module(new M0_Config_ROM(N,r,base_r,ptype,w)).io
    val M1_config = Module(new M1_Config_ROM(N,r,base_r, ptype, w)).io
    val Perm_Config = Module(new Streaming_Permute_Config(N,r, base_r, ptype, w)).io
    val cnt = RegInit(0.U(log2Ceil(N/w).W))
    when(io.in_en.asUInt.orR){
      when(cnt === ((N/w)-1).U){
        cnt := 0.U
        offset_switch := ~offset_switch
      }.otherwise{
        cnt := cnt + 1.U
        offset_switch := offset_switch
      }
      for(i <- 0 until w){
        M0(i).en := true.B
        M0(i).re := true.B
        M0(i).wr := true.B
        M1(i).en := true.B
        M1(i).re := true.B
        M1(i).wr := true.B
        M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch)
        M0(i).in_waddr := cnt + (offset * offset_switch)
        M0(i).in_data := io.in(i)
        M1(i).in_raddr := cnt + (offset * ~offset_switch)
        M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
        M1(i).in_data := Perm_Mod.out(i)
        Perm_Mod.in_config(i) := Perm_Config.out(i)
        Perm_Mod.in(i) := M0(i).out_data
        Perm_Mod.in_en := true.B
        io.out(i) := M1(i).out_data
      }
    }.otherwise{
      for(i <- 0 until w){
        M0(i).en := false.B
        M0(i).re := false.B
        M0(i).wr := false.B
        M1(i).en := false.B
        M1(i).re := false.B
        M1(i).wr := false.B
        M0(i).in_raddr := 0.U
        M0(i).in_waddr := 0.U
        M0(i).in_data := io.in(i)
        M1(i).in_raddr := 0.U
        M1(i).in_waddr := 0.U
        M1(i).in_data := Perm_Mod.out(i)
        Perm_Mod.in_config(i) := Perm_Config.out(i)
        Perm_Mod.in(i) := M0(i).out_data
        Perm_Mod.in_en := false.B
        io.out(i) := M1(i).out_data
      }
      offset_switch := 0.U
      cnt := 0.U
    }
    M0_config.in_cnt := cnt
    M1_config.in_cnt := cnt
    Perm_Config.in_cnt := cnt
  }

  class PermutationsWithStreaming_v2(N: Int, r: Int, base_r: Int, w: Int, ptype: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in_en_main = Input(Bool())
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec((N / w) * 2 + 1, Bool()))
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val is_enabled = RegInit(false.B)
    val offset = (N / w).U
    val offset_switch = RegInit(1.U(1.W))
    when(io.in_en_main) {
      when(io.in_en.asUInt.orR) {
        is_enabled := true.B
      }.otherwise {
        is_enabled := false.B
      }
    }

    val M0 = (for (i <- 0 until w) yield {
      val M0_t = Module(new RAM_Block(N, w, bw)).io
      M0_t
    }).toVector

    val M1 = (for (i <- 0 until w) yield {
      val M1_t = Module(new RAM_Block(N, w, bw)).io
      M1_t
    }).toVector

    for (i <- 0 until w) {
      M0(i).en := io.in_en_main & io.in_en.asUInt.orR
      M1(i).en := io.in_en_main & io.in_en.asUInt.orR
    }
    val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io
    val M0_config = Module(new M0_Config_ROM(N, r, base_r, ptype, w)).io
    val M1_config = Module(new M1_Config_ROM(N, r, base_r, ptype, w)).io
    val Perm_Config = Module(new Streaming_Permute_Config(N, r, base_r, ptype, w)).io
    val cnt = RegInit(0.U(log2Ceil(N / w).W))
    when(io.in_en_main) {
      when(io.in_en.asUInt.orR) {
        when(cnt === ((N / w) - 1).U) {
          cnt := 0.U
          offset_switch := ~offset_switch
        }.otherwise {
          cnt := cnt + 1.U
          offset_switch := offset_switch
        }
        for (i <- 0 until w) {
          M0(i).re := true.B
          M0(i).wr := true.B
          M1(i).re := true.B
          M1(i).wr := true.B
          M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch)
          M0(i).in_waddr := cnt + (offset * offset_switch)
          M0(i).in_data := io.in(i)
          M1(i).in_raddr := cnt + (offset * ~offset_switch)
          M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data
          Perm_Mod.in_en := true.B
          io.out(i) := M1(i).out_data
        }
      }.otherwise {
        for (i <- 0 until w) {
          M0(i).re := false.B
          M0(i).wr := false.B
          M1(i).re := false.B
          M1(i).wr := false.B
          M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch)
          M0(i).in_waddr := cnt + (offset * offset_switch)
          M0(i).in_data := io.in(i)
          M1(i).in_raddr := cnt + (offset * ~offset_switch)
          M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data
          Perm_Mod.in_en := false.B
          io.out(i) := M1(i).out_data
        }
//        offset_switch := 0.U
//        cnt := 0.U
      }
    }.otherwise{
      when(io.in_en.asUInt.orR){
        for (i <- 0 until w) {
          M0(i).re := true.B
          M0(i).wr := true.B
          M1(i).re := true.B
          M1(i).wr := true.B
          M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch)
          M0(i).in_waddr := cnt + (offset * offset_switch)
          M0(i).in_data := io.in(i)
          M1(i).in_raddr := cnt + (offset * ~offset_switch)
          M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data
          Perm_Mod.in_en := true.B
          io.out(i) := M1(i).out_data
        }
      }.otherwise{
        for (i <- 0 until w) {
          M0(i).re := false.B
          M0(i).wr := false.B
          M1(i).re := false.B
          M1(i).wr := false.B
          M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch)
          M0(i).in_waddr := cnt + (offset * offset_switch)
          M0(i).in_data := io.in(i)
          M1(i).in_raddr := cnt + (offset * ~offset_switch)
          M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data
          Perm_Mod.in_en := false.B
          io.out(i) := M1(i).out_data
        }
      }
    }
    M0_config.in_cnt := cnt
    M1_config.in_cnt := cnt
    Perm_Config.in_cnt := cnt
  }

  class PermutationsWithStreaming_mr(N:Int, r: Int, base_r: Int, w:Int, win:Int, ptype: Int, bw: Int, delay: Int) extends Module{
    val ccs = if(win>w){N/w}else{N/win}
    val io = IO(new Bundle() {
      val in = Input(Vec(win, new ComplexNum(bw))) // win inputs at time
      val in_en = Input(Vec(ccs*2 + 1, Bool())) // it seems that latency will be around (N/win)*2
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val is_enabled = Reg(Bool()) // not used anymore
    val offset = (N/w).U
    val offset_switch = Reg(UInt(1.W))
    if(win < w) {
      val M0 = VecInit(for(i <- 0 until w) yield{ //has w memory ports, each with N/w * 2 addresses
        val M0_t = Module(new RAM_Block(N,w,bw)).io
        M0_t
      })
      val M1 = VecInit(for(i <- 0 until w) yield{ // same as M1 //lets make it a vec
        val M1_t = Module(new RAM_Block(N,w,bw)).io // this should be valid since all the parameters are the same
        M1_t
      })
      val modded_list = for (i <- 0 until N/w) yield { // setting up a mapping for writing into the M0 block
        for (j <- 0 until w) yield {
          (i, j)
        }
      }
      val modded_list2 = modded_list.flatten
      val modded_list3 = for (i <- 0 until N/win) yield {// too large of an array was causing problems, in reality it should be (N/win)
        for (j <- 0 until win) yield {
          modded_list2(i * win + j)
        }
      }
      val M0_access_addr = modded_list3.map(x => x.map(y => y._1))
      val M0_access_port = modded_list3.map(x => x.map(y => y._2))
      val M0_vec_addr = for (i <- 0 until N/win) yield {
        val V = VecInit(M0_access_addr(i).map(_.U((log2Ceil((N/win) * 2)).W)))
        V
      }
      val M0_vec_port = for (i <- 0 until N/win) yield {
        val V = VecInit(M0_access_port(i).map(_.U((log2Ceil((w) * 2)).W)))
        V
      }
      val vvf_addr = VecInit(M0_vec_addr) // used to write into the M0 memory block , there are N/win blocks with win elements each
      val vvf_port = VecInit(M0_vec_port)

      val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io // nothing here should change
      val M0_config = Module(new M0_Config_ROM(N, r, base_r, ptype, w)).io // this should not change
      val M1_config = Module(new M1_Config_ROM(N, r, base_r, ptype, w)).io
      val Perm_Config = Module(new Streaming_Permute_Config(N, r, base_r, ptype, w)).io

      val cnt2 = RegInit(0.U(log2Ceil(N / w).W)) // secondary counter
      val cnt = RegInit(0.U(log2Ceil(N / win).W)) // this counter is the primary  counter // just need slightly more time to input the values into M0
      when(io.in_en.asUInt.orR) {
        when(cnt2 === ((N / w) - 1).U && cnt === ((N / win) - 1).U) { // cnt must have reached the end for the counters to start over
          cnt2 := 0.U
          cnt := 0.U
          offset_switch := ~offset_switch
        }.elsewhen(cnt2 === ((N / w) - 1).U) { // if cnt has not reached the end but cnt2 is at the end
          cnt2 := cnt2 // cnt2 stops momentarily to prevent any changes
          cnt := cnt + 1.U
          offset_switch := offset_switch
        }.otherwise { // if none of the counters have counted to the end
          cnt2 := cnt2 + 1.U
          cnt := cnt + 1.U
          offset_switch := offset_switch
        }
        for (i <- 0 until w) {
          M0(i).en := true.B // enable the memory blocks
          M0(i).re := true.B
          M1(i).en := true.B
          M1(i).re := true.B
          M1(i).wr := true.B
          M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch) // read w elements from the M0 block each cc to go into the perm modules
          M1(i).in_raddr := cnt2 + (offset * ~offset_switch)
          M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data // still outputs data at N/w
          Perm_Mod.in_en := true.B
          io.out(i) := M1(i).out_data
        }
        for (i <- 0 until w) {
          M0(i).wr := false.B
          M0(i).in_waddr := 0.U
          M0(i).in_data := 0.U.asTypeOf(new ComplexNum(bw))
        }
        for (i <- 0 until w) {
          for (j <- 0 until win) {
            when(vvf_port(cnt)(j) === i.U) {
              M0(i).wr := true.B
              M0(i).in_waddr := vvf_addr(cnt)(j) + (offset * offset_switch)
              M0(i).in_data := io.in(j)
            }
          }
        }
      }.otherwise {
        for (i <- 0 until w) {
          M0(i).en := false.B
          M0(i).re := false.B
          M0(i).wr := false.B
          M1(i).en := false.B
          M1(i).re := false.B
          M1(i).wr := false.B
          M0(i).in_raddr := 0.U
          M0(i).in_waddr := 0.U
          M0(i).in_data := 0.U.asTypeOf(new ComplexNum(bw))
          M1(i).in_raddr := 0.U
          M1(i).in_waddr := 0.U
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data
          Perm_Mod.in_en := false.B
          io.out(i) := M1(i).out_data
        }
        offset_switch := 0.U
        cnt := cnt // no change
        cnt2 := cnt2
      }
      M0_config.in_cnt := cnt2 // still depends on cnt2, it is just the input that is affected by cnt
      M1_config.in_cnt := cnt2
      Perm_Config.in_cnt := cnt2
    }else{ // otherwise the win is greater than the w
      val input_delay_cycles = N/w - N/win // win is greater than w
      val input_delay_registers = RegInit(VecInit.fill(input_delay_cycles)(VecInit.fill(win)(0.U.asTypeOf(new ComplexNum(bw))))) // delaying the inputs to line up the ccs with N/w
      val counter_delay_registers = RegInit(VecInit.fill(input_delay_cycles)(false.B))
      val M0 = VecInit(for(i <- 0 until w) yield{ //has w memory ports, each with N/w * 2 addresses
        val M0_t = Module(new RAM_Block_MultiWrite(N,w,(win.toDouble/w.toDouble).ceil.toInt,bw)).io
        M0_t
      })
      val M1 = VecInit(for(i <- 0 until w) yield{ // same as M1 //lets make it a vec
        val M1_t = Module(new RAM_Block(N,w,bw)).io // this should be valid since all the parameters are the same
        M1_t
      })
      val modded_list4 = for(i <- 0 until N/w) yield{
        for(j <- 0 until w) yield{
          (i,j)
        }
      }
      val modded_list5 = modded_list4.flatten
      val modded_list6 = for(i <- 0 until N/win) yield{
        for(j <- 0 until win) yield{
          modded_list5(i*(win)+ j)
        }
      }
      val mem_enables = mutable.ArrayBuffer[Array[Array[Boolean]]]()
      val mem_addr_adj = mutable.ArrayBuffer[Array[Array[Int]]]()
      val mem_input_pass = mutable.ArrayBuffer[Array[Array[Int]]]()
      for(i <- 0 until w){
        val temp = mutable.ArrayBuffer[Array[Boolean]]()
        val temp_ports1 = mutable.ArrayBuffer[Array[Int]]()
        val temp_addr1 = mutable.ArrayBuffer[Array[Int]]()
        val temp_inp1 = mutable.ArrayBuffer[Array[Int]]()
        for(j  <- 0 until N/win){
          val temp2 = mutable.ArrayBuffer[Boolean]()
          val temp_ports2 = mutable.ArrayBuffer[Int]()
          val temp_addr2 = mutable.ArrayBuffer[Int]()
          val temp_inp2 = mutable.ArrayBuffer[Int]()
          for(k <- 0 until win){
            if(modded_list6(j)(k)._2 == i){
              temp2 += true
              temp_ports2 += modded_list6(j)(k)._2
              temp_addr2 += modded_list6(j)(k)._1
              temp_inp2 += k
            }
          }
          for(k <- temp2.length until (win.toDouble/w.toDouble).ceil.toInt){
            temp2 += false
            temp_ports2 += 0
            temp_addr2 += 0
            temp_inp2 += 0
          }
          temp += temp2.toArray
          temp_ports1 += temp_ports2.toArray
          temp_addr1 += temp_addr2.toArray
          temp_inp1 += temp_inp2.toArray
        }
        mem_enables += temp.toArray
        mem_addr_adj += temp_addr1.toArray
        mem_input_pass += temp_inp1.toArray
      }


      val M0_mem_wen = VecInit(mem_enables.map(y=>VecInit(y.map(z=>VecInit(z.map(_.B))))).toArray) // this table contains all the write enables for each port
      val M0_addr = VecInit(mem_addr_adj.map(y=>VecInit(y.map(z=>VecInit(z.map(_.U(log2Ceil(N/w).W)))))).toArray)
      val M0_inp_order = VecInit(mem_input_pass.map(y=>VecInit(y.map(z=>VecInit(z.map(_.U(log2Ceil(win).W)))))).toArray)

      val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io // nothing here should change
      val M0_config = Module(new M0_Config_ROM(N, r, base_r, ptype, w)).io // this should not change
      val M1_config = Module(new M1_Config_ROM(N, r, base_r, ptype, w)).io
      val Perm_Config = Module(new Streaming_Permute_Config(N, r, base_r, ptype, w)).io

      val cnt2 = RegInit(0.U(log2Ceil(N / w).W)) // secondary counter
      val cnt = RegInit(0.U(log2Ceil(N / win).W)) // this counter is the primary  counter // just need slightly more time to input the values into M0 // since the win is now larger than the w, we will need to delay the counter some cycles before it begins to count
      when(io.in_en.asUInt.orR) {
        for(i <- 0 until input_delay_cycles){
          if(i == 0){
            counter_delay_registers(0) := true.B
          }else{
            counter_delay_registers(i) := counter_delay_registers(i-1)
          }
        }
        when(cnt2 === ((N / w) - 1).U && cnt === ((N / win) - 1).U) { // cnt must have reached the end for the counters to start over // need to be a little careful here since the primary counter will reach the end of the counter stages sooner than the descondary
          cnt2 := 0.U
          cnt := 0.U
          offset_switch := ~offset_switch
        }.elsewhen(cnt === ((N / win) - 1).U) { // if cnt has not reached the end but cnt2 is at the end
          cnt := cnt // cnt2 stops momentarily to prevent any changes
          cnt2 := cnt2 + 1.U
          offset_switch := offset_switch
        }.otherwise { // if none of the counters have counted to the end
          cnt2 := cnt2 + 1.U
          when(cnt2 >= delay.U){
            cnt := cnt + 1.U
          }.otherwise{
            cnt := 0.U
          }
          offset_switch := offset_switch
        }
        for (i <- 0 until w) {
          M0(i).en := true.B // enable the memory blocks
          M0(i).re := true.B
          M1(i).en := true.B
          M1(i).re := true.B
          M1(i).wr := true.B
          M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch) // read w elements from the M0 block each cc to go into the perm modules
          M1(i).in_raddr := cnt2 + (offset * ~offset_switch)
          M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data // still outputs data at N/w
          Perm_Mod.in_en := true.B
          io.out(i) := M1(i).out_data
        }
        for(i <- 0 until input_delay_cycles){
          if(i == 0){
            input_delay_registers(0) := io.in
          }else{
            input_delay_registers(i) := input_delay_registers(i-1)
          }
        }
        for(i <- 0 until w){
          for(k <- 0 until (win.toDouble/w.toDouble).ceil.toInt){
            M0(i).wr(k) := M0_mem_wen(i)(cnt)(k) //this provides the enables each clock cycle
            M0(i).in_waddr(k) := M0_addr(i)(cnt)(k) + (offset * offset_switch)
            M0(i).in_data(k) := input_delay_registers(input_delay_cycles-1)(M0_inp_order(i)(cnt)(k))
          }
        }
      }.otherwise {
        input_delay_registers := VecInit.fill(input_delay_cycles)(VecInit.fill(win)(0.U.asTypeOf(new ComplexNum(bw))))
        counter_delay_registers := VecInit.fill(input_delay_cycles)(false.B)
        for (i <- 0 until w) {
          M0(i).en := false.B
          M0(i).re := false.B
          M0(i).wr := VecInit.fill((win.toDouble/w.toDouble).ceil.toInt)(false.B)
          M1(i).en := false.B
          M1(i).re := false.B
          M1(i).wr := false.B
          M0(i).in_raddr := 0.U
          M0(i).in_waddr := VecInit.fill((win.toDouble/w.toDouble).ceil.toInt)(0.U)
          M0(i).in_data := VecInit.fill((win.toDouble/w.toDouble).ceil.toInt)(0.U.asTypeOf(new ComplexNum(bw)))
          M1(i).in_raddr := 0.U
          M1(i).in_waddr := 0.U
          M1(i).in_data := Perm_Mod.out(i)
          Perm_Mod.in_config(i) := Perm_Config.out(i)
          Perm_Mod.in(i) := M0(i).out_data
          Perm_Mod.in_en := false.B
          io.out(i) := M1(i).out_data
        }
        offset_switch := 0.U
        cnt := 0.U // no change
        cnt2 := 0.U
      }
      M0_config.in_cnt := cnt2 // still depends on cnt2, it is just the input that is affected by cnt
      M1_config.in_cnt := cnt2
      Perm_Config.in_cnt := cnt2
    }
  }

  class PermutationsWithStreaming_mr_v2(N: Int, r: Int, base_r: Int, w: Int, win: Int, ptype: Int, bw: Int, delay: Int) extends Module {
    val ccs = if (win > w) {
      N / w
    } else {
      N / win
    }
    val io = IO(new Bundle() {
      val in_en_main = Input(Bool())
      val in = Input(Vec(win, new ComplexNum(bw))) // win inputs at time
      val in_en = Input(Vec(ccs * 2 + 1, Bool())) // it seems that latency will be around (N/win)*2
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val is_enabled = RegInit(false.B) // not used anymore
    val offset = (N / w).U
    val offset_switch = RegInit(0.U(1.W))
    if (win < w) {
      val M0 = (for (i <- 0 until w) yield { //has w memory ports, each with N/w * 2 addresses
        val M0_t = Module(new RAM_Block(N, w, bw)).io
        M0_t
      }).toVector
      val M1 = (for (i <- 0 until w) yield { // same as M1 //lets make it a vec
        val M1_t = Module(new RAM_Block(N, w, bw)).io // this should be valid since all the parameters are the same
        M1_t
      }).toVector
      for(i <- 0 until  w){
        M0(i).en := io.in_en_main & io.in_en.asUInt.orR
        M1(i).en := io.in_en_main & io.in_en.asUInt.orR
      }
      val modded_list = for (i <- 0 until N / w) yield { // setting up a mapping for writing into the M0 block
        for (j <- 0 until w) yield {
          (i, j)
        }
      }
      val modded_list2 = modded_list.flatten
      val modded_list3 = for (i <- 0 until N / win) yield { // too large of an array was causing problems, in reality it should be (N/win)
        for (j <- 0 until win) yield {
          modded_list2(i * win + j)
        }
      }
      val M0_access_addr = modded_list3.map(x => x.map(y => y._1))
      val M0_access_port = modded_list3.map(x => x.map(y => y._2))
      val M0_vec_addr = for (i <- 0 until N / win) yield {
        val V = VecInit(M0_access_addr(i).map(_.U((log2Ceil((N / win) * 2)).W)))
        V
      }
      val M0_vec_port = for (i <- 0 until N / win) yield {
        val V = VecInit(M0_access_port(i).map(_.U((log2Ceil((w) * 2)).W)))
        V
      }
      val vvf_addr = VecInit(M0_vec_addr) // used to write into the M0 memory block , there are N/win blocks with win elements each
      val vvf_port = VecInit(M0_vec_port)

      val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io // nothing here should change
      val M0_config = Module(new M0_Config_ROM(N, r, base_r, ptype, w)).io // this should not change
      val M1_config = Module(new M1_Config_ROM(N, r, base_r, ptype, w)).io
      val Perm_Config = Module(new Streaming_Permute_Config(N, r, base_r, ptype, w)).io

      val cnt2 = RegInit(0.U(log2Ceil(N / w).W)) // secondary counter
      val cnt = RegInit(0.U(log2Ceil(N / win).W)) // this counter is the primary  counter // just need slightly more time to input the values into M0
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          when(cnt2 === ((N / w) - 1).U && cnt === ((N / win) - 1).U) { // cnt must have reached the end for the counters to start over
            cnt2 := 0.U
            cnt := 0.U
            offset_switch := ~offset_switch
          }.elsewhen(cnt2 === ((N / w) - 1).U) { // if cnt has not reached the end but cnt2 is at the end
            cnt2 := cnt2 // cnt2 stops momentarily to prevent any changes
            cnt := cnt + 1.U
            offset_switch := offset_switch
          }.otherwise { // if none of the counters have counted to the end
            cnt2 := cnt2 + 1.U
            cnt := cnt + 1.U
            offset_switch := offset_switch
          }
          for (i <- 0 until w) {
            M0(i).re := true.B
            M1(i).re := true.B
            M1(i).wr := true.B
            M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch) // read w elements from the M0 block each cc to go into the perm modules
            M1(i).in_raddr := cnt2 + (offset * ~offset_switch)
            M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data // still outputs data at N/w
            Perm_Mod.in_en := true.B
            io.out(i) := M1(i).out_data
          }
          for (i <- 0 until w) {
            M0(i).wr := false.B
            M0(i).in_waddr := 0.U
            M0(i).in_data := 0.U.asTypeOf(new ComplexNum(bw))
          }
          for (i <- 0 until w) {
            for (j <- 0 until win) {
              when(vvf_port(cnt)(j) === i.U) {
                M0(i).wr := true.B
                M0(i).in_waddr := vvf_addr(cnt)(j) + (offset * offset_switch)
                M0(i).in_data := io.in(j)
              }
            }
          }
        }.otherwise {
          for (i <- 0 until w) {
            M0(i).re := false.B
            M0(i).wr := false.B
            M1(i).re := false.B
            M1(i).wr := false.B
            M0(i).in_raddr := 0.U
            M0(i).in_waddr := 0.U
            M0(i).in_data := 0.U.asTypeOf(new ComplexNum(bw))
            M1(i).in_raddr := 0.U
            M1(i).in_waddr := 0.U
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data
            Perm_Mod.in_en := false.B
            io.out(i) := M1(i).out_data
          }
//          cnt := cnt // no change
//          cnt2 := cnt2
        }
      }.otherwise{
        when(io.in_en.asUInt.orR){
          for (i <- 0 until w) {
            M0(i).re := true.B
            M1(i).re := true.B
            M1(i).wr := true.B
            M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch) // read w elements from the M0 block each cc to go into the perm modules
            M1(i).in_raddr := cnt2 + (offset * ~offset_switch)
            M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data // still outputs data at N/w
            Perm_Mod.in_en := true.B
            io.out(i) := M1(i).out_data
          }
          for (i <- 0 until w) {
            M0(i).wr := false.B
            M0(i).in_waddr := 0.U
            M0(i).in_data := 0.U.asTypeOf(new ComplexNum(bw))
          }
          for (i <- 0 until w) {
            for (j <- 0 until win) {
              when(vvf_port(cnt)(j) === i.U) {
                M0(i).wr := true.B
                M0(i).in_waddr := vvf_addr(cnt)(j) + (offset * offset_switch)
                M0(i).in_data := io.in(j)
              }
            }
          }
        }.otherwise{
          for (i <- 0 until w) {
            M0(i).re := false.B
            M0(i).wr := false.B
            M1(i).re := false.B
            M1(i).wr := false.B
            M0(i).in_raddr := 0.U
            M0(i).in_waddr := 0.U
            M0(i).in_data := 0.U.asTypeOf(new ComplexNum(bw))
            M1(i).in_raddr := 0.U
            M1(i).in_waddr := 0.U
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data
            Perm_Mod.in_en := false.B
            io.out(i) := M1(i).out_data
          }
        }
      }
      M0_config.in_cnt := cnt2 // still depends on cnt2, it is just the input that is affected by cnt
      M1_config.in_cnt := cnt2
      Perm_Config.in_cnt := cnt2
    } else { // otherwise the win is greater than the w
      val input_delay_cycles = N / w - N / win // win is greater than w
      val input_delay_registers = RegInit(VecInit.fill(input_delay_cycles)(VecInit.fill(win)(0.U.asTypeOf(new ComplexNum(bw))))) // delaying the inputs to line up the ccs with N/w
      val counter_delay_registers = RegInit(VecInit.fill(input_delay_cycles)(false.B))
      val M0 = (for (i <- 0 until w) yield { //has w memory ports, each with N/w * 2 addresses
        val M0_t = Module(new RAM_Block_MultiWrite(N, w, (win.toDouble / w.toDouble).ceil.toInt, bw)).io
        M0_t
      }).toVector
      val M1 = VecInit(for (i <- 0 until w) yield { // same as M1 //lets make it a vec
        val M1_t = Module(new RAM_Block(N, w, bw)).io // this should be valid since all the parameters are the same
        M1_t
      }).toVector
      for(i <- 0 until w){
        M0(i).en := io.in_en_main & io.in_en.asUInt.orR
        M1(i).en := io.in_en_main & io.in_en.asUInt.orR
      }
      val modded_list4 = for (i <- 0 until N / w) yield {
        for (j <- 0 until w) yield {
          (i, j)
        }
      }
      val modded_list5 = modded_list4.flatten
      val modded_list6 = for (i <- 0 until N / win) yield {
        for (j <- 0 until win) yield {
          modded_list5(i * (win) + j)
        }
      }
      val mem_enables = mutable.ArrayBuffer[Array[Array[Boolean]]]()
      val mem_addr_adj = mutable.ArrayBuffer[Array[Array[Int]]]()
      val mem_input_pass = mutable.ArrayBuffer[Array[Array[Int]]]()
      for (i <- 0 until w) {
        val temp = mutable.ArrayBuffer[Array[Boolean]]()
        val temp_ports1 = mutable.ArrayBuffer[Array[Int]]()
        val temp_addr1 = mutable.ArrayBuffer[Array[Int]]()
        val temp_inp1 = mutable.ArrayBuffer[Array[Int]]()
        for (j <- 0 until N / win) {
          val temp2 = mutable.ArrayBuffer[Boolean]()
          val temp_ports2 = mutable.ArrayBuffer[Int]()
          val temp_addr2 = mutable.ArrayBuffer[Int]()
          val temp_inp2 = mutable.ArrayBuffer[Int]()
          for (k <- 0 until win) {
            if (modded_list6(j)(k)._2 == i) {
              temp2 += true
              temp_ports2 += modded_list6(j)(k)._2
              temp_addr2 += modded_list6(j)(k)._1
              temp_inp2 += k
            }
          }
          for (k <- temp2.length until (win.toDouble / w.toDouble).ceil.toInt) {
            temp2 += false
            temp_ports2 += 0
            temp_addr2 += 0
            temp_inp2 += 0
          }
          temp += temp2.toArray
          temp_ports1 += temp_ports2.toArray
          temp_addr1 += temp_addr2.toArray
          temp_inp1 += temp_inp2.toArray
        }
        mem_enables += temp.toArray
        mem_addr_adj += temp_addr1.toArray
        mem_input_pass += temp_inp1.toArray
      }


      val M0_mem_wen = VecInit(mem_enables.map(y => VecInit(y.map(z => VecInit(z.map(_.B))))).toArray) // this table contains all the write enables for each port
      val M0_addr = VecInit(mem_addr_adj.map(y => VecInit(y.map(z => VecInit(z.map(_.U(log2Ceil(N / w).W)))))).toArray)
      val M0_inp_order = VecInit(mem_input_pass.map(y => VecInit(y.map(z => VecInit(z.map(_.U(log2Ceil(win).W)))))).toArray)

      val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io // nothing here should change
      val M0_config = Module(new M0_Config_ROM(N, r, base_r, ptype, w)).io // this should not change
      val M1_config = Module(new M1_Config_ROM(N, r, base_r, ptype, w)).io
      val Perm_Config = Module(new Streaming_Permute_Config(N, r, base_r, ptype, w)).io

      val cnt2 = RegInit(0.U(log2Ceil(N / w).W)) // secondary counter
      val cnt = RegInit(0.U(log2Ceil(N / win).W)) // this counter is the primary  counter // just need slightly more time to input the values into M0 // since the win is now larger than the w, we will need to delay the counter some cycles before it begins to count
      when(io.in_en_main) {
        when(io.in_en.asUInt.orR) {
          for (i <- 0 until input_delay_cycles) {
            if (i == 0) {
              counter_delay_registers(0) := true.B
            } else {
              counter_delay_registers(i) := counter_delay_registers(i - 1)
            }
          }
          when(cnt2 === ((N / w) - 1).U && cnt === ((N / win) - 1).U) { // cnt must have reached the end for the counters to start over // need to be a little careful here since the primary counter will reach the end of the counter stages sooner than the descondary
            cnt2 := 0.U
            cnt := 0.U
            offset_switch := ~offset_switch
          }.elsewhen(cnt === ((N / win) - 1).U) { // if cnt has not reached the end but cnt2 is at the end
            cnt := cnt // cnt2 stops momentarily to prevent any changes
            cnt2 := cnt2 + 1.U
            offset_switch := offset_switch
          }.otherwise { // if none of the counters have counted to the end
            cnt2 := cnt2 + 1.U
            when(cnt2 >= delay.U) {
              cnt := cnt + 1.U
            }.otherwise {
              cnt := 0.U
            }
            offset_switch := offset_switch
          }
          for (i <- 0 until w) {
            M0(i).re := true.B
            M1(i).re := true.B
            M1(i).wr := true.B
            M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch) // read w elements from the M0 block each cc to go into the perm modules
            M1(i).in_raddr := cnt2 + (offset * ~offset_switch)
            M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data // still outputs data at N/w
            Perm_Mod.in_en := true.B
            io.out(i) := M1(i).out_data
          }
          for (i <- 0 until input_delay_cycles) {
            if (i == 0) {
              input_delay_registers(0) := io.in
            } else {
              input_delay_registers(i) := input_delay_registers(i - 1)
            }
          }
          for (i <- 0 until w) {
            for (k <- 0 until (win.toDouble / w.toDouble).ceil.toInt) {
              M0(i).wr(k) := M0_mem_wen(i)(cnt)(k) //this provides the enables each clock cycle
              M0(i).in_waddr(k) := M0_addr(i)(cnt)(k) + (offset * offset_switch)
              M0(i).in_data(k) := input_delay_registers(input_delay_cycles - 1)(M0_inp_order(i)(cnt)(k))
            }
          }
        }.otherwise {
//          input_delay_registers := VecInit.fill(input_delay_cycles)(VecInit.fill(win)(0.U.asTypeOf(new ComplexNum(bw))))
//          counter_delay_registers := VecInit.fill(input_delay_cycles)(false.B)
          for (i <- 0 until w) {
            M0(i).re := false.B
            M0(i).wr := VecInit.fill((win.toDouble / w.toDouble).ceil.toInt)(false.B)
            M1(i).re := false.B
            M1(i).wr := false.B
            M0(i).in_raddr := 0.U
            M0(i).in_waddr := VecInit.fill((win.toDouble / w.toDouble).ceil.toInt)(0.U)
            M0(i).in_data := VecInit.fill((win.toDouble / w.toDouble).ceil.toInt)(0.U.asTypeOf(new ComplexNum(bw)))
            M1(i).in_raddr := 0.U
            M1(i).in_waddr := 0.U
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data
            Perm_Mod.in_en := false.B
            io.out(i) := M1(i).out_data
          }
//          offset_switch := 0.U
//          cnt := 0.U // no change
//          cnt2 := 0.U
        }
      }.otherwise{
        when(io.in_en.asUInt.orR){
          for (i <- 0 until w) {
            M0(i).re := true.B
            M1(i).re := true.B
            M1(i).wr := true.B
            M0(i).in_raddr := M0_config.out(i) + (offset * ~offset_switch) // read w elements from the M0 block each cc to go into the perm modules
            M1(i).in_raddr := cnt2 + (offset * ~offset_switch)
            M1(i).in_waddr := M1_config.out(i) + (offset * offset_switch)
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data // still outputs data at N/w
            Perm_Mod.in_en := true.B
            io.out(i) := M1(i).out_data
          }
          for (i <- 0 until w) {
            for (k <- 0 until (win.toDouble / w.toDouble).ceil.toInt) {
              M0(i).wr(k) := M0_mem_wen(i)(cnt)(k) //this provides the enables each clock cycle
              M0(i).in_waddr(k) := M0_addr(i)(cnt)(k) + (offset * offset_switch)
              M0(i).in_data(k) := input_delay_registers(input_delay_cycles - 1)(M0_inp_order(i)(cnt)(k))
            }
          }
        }.otherwise{
          for (i <- 0 until w) {
            M0(i).re := false.B
            M0(i).wr := VecInit.fill((win.toDouble / w.toDouble).ceil.toInt)(false.B)
            M1(i).re := false.B
            M1(i).wr := false.B
            M0(i).in_raddr := 0.U
            M0(i).in_waddr := VecInit.fill((win.toDouble / w.toDouble).ceil.toInt)(0.U)
            M0(i).in_data := VecInit.fill((win.toDouble / w.toDouble).ceil.toInt)(0.U.asTypeOf(new ComplexNum(bw)))
            M1(i).in_raddr := 0.U
            M1(i).in_waddr := 0.U
            M1(i).in_data := Perm_Mod.out(i)
            Perm_Mod.in_config(i) := Perm_Config.out(i)
            Perm_Mod.in(i) := M0(i).out_data
            Perm_Mod.in_en := false.B
            io.out(i) := M1(i).out_data
          }
        }
      }
      M0_config.in_cnt := cnt2
      M1_config.in_cnt := cnt2
      Perm_Config.in_cnt := cnt2
    }
  }

  class PermutationsWithStreaming_and_iterative(N:Int, r: Int, base_r: Int, w:Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_en = Input(Vec((N/w)*2 + 1, Bool()))
      val in_perm = Input(Vec(w, UInt(log2Ceil(N).W)))
      val in_M0_config = Input(Vec(w, UInt(log2Ceil(N).W)))
      val in_M1_config = Input(Vec(w, UInt(log2Ceil(N).W)))
      val out = Output(Vec(w, new ComplexNum(bw)))
      val out_cnt = Output(UInt(log2Ceil(N/w).W))
    })
    val is_enabled = Reg(Bool())
    val offset = (N/w).U
    val offset_switch = Reg(UInt(1.W))
    val M0 = (for(i <- 0 until w) yield{
      val M0_t = Module(new RAM_Block(N,w,bw)).io
      M0_t
    }).toVector
    val M1 = (for(i <- 0 until w) yield{
      val M1_t = Module(new RAM_Block(N,w,bw)).io
      M1_t
    }).toVector
    val Perm_Mod = Module(new Permute_Unit_Streaming(w, bw)).io
    val cnt = RegInit(0.U(log2Ceil(N/w).W))
    when(io.in_en.asUInt.orR){
      when(cnt === ((N/w)-1).U){
        cnt := 0.U
        offset_switch := ~offset_switch
      }.otherwise{
        cnt := cnt + 1.U
        offset_switch := offset_switch
      }
      for(i <- 0 until w){
        M0(i).en := true.B
        M0(i).re := true.B
        M0(i).wr := true.B
        M1(i).en := true.B
        M1(i).re := true.B
        M1(i).wr := true.B
        M0(i).in_raddr := io.in_M0_config(i) + (offset * ~offset_switch)
        M0(i).in_waddr := cnt + (offset * offset_switch)
        M0(i).in_data := io.in(i)
        M1(i).in_raddr := cnt + (offset * ~offset_switch)
        M1(i).in_waddr := io.in_M1_config(i) + (offset * offset_switch)
        M1(i).in_data := Perm_Mod.out(i)
        Perm_Mod.in_config(i) := io.in_perm(i)
        Perm_Mod.in(i) := M0(i).out_data
        Perm_Mod.in_en := true.B
        io.out(i) := M1(i).out_data
      }
    }.otherwise{
      for(i <- 0 until w){
        M0(i).en := false.B
        M0(i).re := false.B
        M0(i).wr := false.B
        M1(i).en := false.B
        M1(i).re := false.B
        M1(i).wr := false.B
        M0(i).in_raddr := 0.U
        M0(i).in_waddr := 0.U
        M0(i).in_data := io.in(i)
        M1(i).in_raddr := 0.U
        M1(i).in_waddr := 0.U
        M1(i).in_data := Perm_Mod.out(i)
        Perm_Mod.in_config(i) := io.in_perm(i)
        Perm_Mod.in(i) := M0(i).out_data
        Perm_Mod.in_en := false.B
        io.out(i) := M1(i).out_data
      }
      offset_switch := 0.U
      cnt := 0.U
    }
    io.out_cnt := cnt
  }

  class Permute_Unit_Streaming(w: Int, bw: Int) extends Module {
    val io = IO(new Bundle() {
      val in = Input(Vec(w, new ComplexNum(bw)))
      val in_config = Input(Vec(w, UInt(log2Ceil(w).W)))
      val in_en = Input(Bool())
      val out = Output(Vec(w, new ComplexNum(bw)))
    })
    val hotvals = for(i <- 0 until w)yield{
      for(j <- 0 until w)yield{
        val hv = (io.in_config(j) === i.U, j.U)
        hv
      }
    }
    val pms = for(i <- 0 until w) yield{
      val pmx = PriorityMux(hotvals(i))
      pmx
    }
    for(i <- 0 until w){
      io.out(i) := io.in(pms(i))
    }
  }

  class M0_Config_ROM(N: Int, r: Int, base_r: Int, ptype: Int, w: Int) extends Module{
    val io = IO(new Bundle() {
      val in_cnt = Input(UInt((log2Ceil(N/w).W)))
      val out = Output(Vec(w, UInt((log2Ceil((N/w) * 2)).W)))
    })
    val cnt = RegInit(0.U(log2Ceil(N/w).W))
    var generated_address = Permutations.GenerateStreamingMappingV2(N,w,r,base_r, ptype)
    generated_address = Permutations.Mapping_Sort(generated_address,0)
    val M0_access = (for(i <- 0 until N/w)yield{
      val M0_slice = (for(j <- 0 until w)yield{
        generated_address(i)(j)._1._1
      }).toArray
      M0_slice
    }).toArray
    val M0_vec = for(i <- 0 until N/w) yield{
      val V = VecInit(M0_access(i).map(_.U((log2Ceil((N/w) * 2)).W)))
      V
    }
    val vvf = VecInit(M0_vec)
    for(i <- 0 until w){
      io.out(i) := vvf(io.in_cnt)(i) //+ offset.U((log2Ceil((N/w) * 2)).W)
    }
  }

  class M1_Config_ROM(N: Int, r: Int, base_r: Int, ptype: Int, w: Int) extends Module{
    val io = IO(new Bundle() {
      val in_cnt = Input(UInt((log2Ceil(N/w).W)))
      val out = Output(Vec(w, UInt((log2Ceil((N/w) * 2)).W)))
    })
    var generated_address = Permutations.GenerateStreamingMappingV2(N,w,r,base_r, ptype)
    generated_address = Permutations.Mapping_Sort(generated_address,1)
    val M1_access = (for(i <- 0 until N/w)yield{
      val M1_slice = (for(j <- 0 until w)yield{
        generated_address(i)(j)._2._1
      }).toArray
      M1_slice
    }).toArray
    val M1_vec = for(i <- 0 until N/w) yield{
      val V = VecInit(M1_access(i).map(_.U((log2Ceil((N/w) * 2)).W)))
      V
    }
    val vvf = VecInit(M1_vec)
    for(i <- 0 until w){
      io.out(i) := vvf(io.in_cnt)(i) //+ offset.U((log2Ceil((N/w) * 2)).W)
    }
  }

  class Streaming_Permute_Config(N: Int, r: Int, base_r: Int, ptype: Int, w: Int)extends Module{
    val io = IO(new Bundle() {
      val in_cnt = Input(UInt((log2Ceil(N/w).W)))
      val out = Output(Vec(w, UInt((log2Ceil(w)).W)))
    })
    var generated_address = Permutations.GenerateStreamingMappingV2(N,w,r,base_r, ptype)
    generated_address = Permutations.Mapping_Sort(generated_address,0)
    val M1_access = (for(i <- 0 until N/w)yield{
      val M1_slice = (for(j <- 0 until w)yield{
        generated_address(i)(j)._2._2
      }).toArray
      M1_slice
    }).toArray
    val M1_vec = for(i <- 0 until N/w) yield{
      val V = VecInit(M1_access(i).map(_.U((log2Ceil(w).W))))
      V
    }
    val vvf = VecInit(M1_vec)
    for(i <- 0 until w){
      io.out(i) := vvf(io.in_cnt)(i)
    }
  }

  class RAM_Block(N: Int, w: Int, bw: Int) extends Module{
    val io = IO(new Bundle() {
      val in_raddr = Input(UInt((log2Ceil(2 * (N/w))).W))
      val in_waddr = Input(UInt((log2Ceil(2 * (N/w))).W))
      val in_data = Input(new ComplexNum(bw))
      val re = Input(Bool())
      val wr = Input(Bool())
      val en = Input(Bool())
      val out_data = Output(new ComplexNum(bw))
    })
        val mem = Reg(Vec((N/w)*2, new ComplexNum(bw)))
//    val mem = RegInit(VecInit.fill((N/w)*2)(0.U.asTypeOf(new ComplexNum(bw))))
    val out_reg_save = RegInit(0.U.asTypeOf(new ComplexNum(bw)))
    when(io.en){
      when(io.wr){
        mem(io.in_waddr) := io.in_data
      }
      when(io.re){
        io.out_data := mem(io.in_raddr)
        out_reg_save := mem(io.in_raddr)
      }.otherwise{
        io.out_data := out_reg_save
      }
    }.otherwise{
      io.out_data := out_reg_save
    }
  }

  class RAM_Block_MultiWrite(N: Int, w: Int, size:Int, bw: Int) extends Module{ //application specific
    val io = IO(new Bundle() {
      val in_raddr = Input(UInt((log2Ceil(2 * (N/w))).W))
      val in_waddr = Input(Vec(size ,UInt((log2Ceil(2 * (N/w))).W)))
      val in_data = Input(Vec(size ,new ComplexNum(bw)))
      val re = Input(Bool())
      val wr = Vec(size ,Input(Bool())) // enable for writes
      val en = Input(Bool())
      val out_data = Output(new ComplexNum(bw))
    })
    val mem = Reg(Vec((N/w)*2, new ComplexNum(bw)))
//      val mem = RegInit(VecInit.fill((N/w)*2)(0.U.asTypeOf(new ComplexNum(bw))))
    val out_reg_save = RegInit(0.U.asTypeOf(new ComplexNum(bw)))

    when(io.en){
      for(i <- 0 until size){
        when(io.wr(i)){
          mem(io.in_waddr(i)) := io.in_data(i)
        }
      }
      when(io.re){
        io.out_data := mem(io.in_raddr)
        out_reg_save := mem(io.in_raddr)
      }.otherwise{
        io.out_data := out_reg_save
      }
    }.otherwise{
      io.out_data := out_reg_save
    }
  }
}
