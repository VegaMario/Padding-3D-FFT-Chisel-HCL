package ChiselFFT
import IEEEConversions.FPConvert._
import FloatingPointDesigns.FPArithmetic._
import ComplexModules.FPComplex._
import chisel3.tester.RawTester.test
import chisel3._
import chisel3.tester._
import FFTSRDesigns._
import FFTMRDesigns._
import DFTDesigns._
import TwidFactorDesigns._
import PermutationDesigns._
import SWFFT.FFT._
import SWFFT.ComplexNumbers._
import SWFFT._
import GoldenModels.FFT_GoldenModels._

import java.io.PrintWriter
import scala.util.Random
import GoldenModels.FFT_GoldenModels._
import Chisel.{isPow2, log2Ceil}

import scala.collection.mutable

object Testing {
  def testing_streaming_mr(N: Int, nr: Int, ns: Int, r: Int, s: Int, w: Int, bw: Int): Unit ={
    test(new FFT_MixedRadix_Streaming(N,nr,ns,r,s, w, bw)){ c=>
      val w1 = w // this will be the input and output width
      var w2 = Permutations.getw2(w1,N,nr,ns,s)
      val CMultLatency = 2
      val T_L = CMultLatency
      var fftlatency1 = 0
      var fftlatency2 = 0
      val perm_latency1 = (N/w1)*2
      if(w1 < nr && w2 < ns){
        fftlatency1 = getfftstreamedlatency(nr,r,w1,bw)
        fftlatency2 = getfftstreamedlatency(ns,s,w2,bw)
      }else if(w1 < nr && w2 >= ns){
        fftlatency1 = getfftstreamedlatency(nr,r,w1,bw) // in this case the fft1 is reduced streaming width
        fftlatency2 = getFFTLatency(ns,s,ns,bw)
      }else if(w1 >= nr && w2 < ns){
        fftlatency1 = getFFTLatency(nr,r,nr,bw) // the fft1 is full streaming width
        fftlatency2 = getfftstreamedlatency(ns,s,w2,bw)
      }else if(w1 >= nr && w2 >= ns){
        fftlatency1 = getFFTLatency(nr,r,nr,bw) // this function should also give the DFT_NRV latency as well if we set the nr==r
        fftlatency2 = getFFTLatency(ns,s,ns,bw)
      }
      val total_latency = perm_latency1*3 + T_L + fftlatency1 + fftlatency2 + 1
      c.io.in_ready.poke(true.B)
      for (i <- 0 until 100) {
        println(s"Clock Cycle: ${i}")
        for (j <- 0 until w1) {
          c.io.in(j).Re.poke(convert_string_to_IEEE_754((((i * w1 + j )%N) + 1).toDouble.toString, bw).U)
          c.io.in(j).Im.poke(convert_string_to_IEEE_754("0.0", bw).U)
        }
        for (j <- 0 until w1) {
          println(s"Real Output: ${convert_long_to_float(c.io.out(j).Re.peek().litValue, bw)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out(j).Im.peek().litValue, bw)}")
        }
        println("_--_--_-_--_-_---__-----__--___--__-")
        c.clock.step(1)
      }
    }
  }

  def testing_iterations(N: Int, r: Int, depth: Int, width: Int, bw: Int ): Unit = {
    test(new FFT_SingleRadix_Streaming_and_Iterative(N,r,depth,width,bw)){ c=>
      val DFTr_Constants = FFT.DFT_gen(r).map(_.toVector).toVector
      var mult_count = 0
      for(i <- 0 until r-1){
        for(j <- 0 until r-1){
          val n  = DFTr_Constants(i+1)(j+1)
          val c1 = FFT.isReducable(n.re.abs)
          val c2 = FFT.isReducable(n.im.abs)
          if(!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
            mult_count += 1
          }
        }
      }
      val inner_square_size = r-1
      var last_ind = r-1
      var start_ind = 1
      val inner_inner_square_size = inner_square_size/2
      val ind_sq = for(i <- 0 until inner_inner_square_size)yield{
        val row = for(j <- 0 until inner_inner_square_size)yield{
          if(((i+1)*(j+1) % r) > r/2){((i+1)*(j+1) % r) - r}else{((i+1)*(j+1) % r)}
        }
        row
      }
      ind_sq.map(x=>println(x))
      val ind_sq_unique = mutable.ArrayBuffer[Int]()
      val ind_sq_unique_address = mutable.ArrayBuffer[(Int, Int)]()
      val cases = mutable.ArrayBuffer[(Int,Int)]()
      val cases_addr = mutable.ArrayBuffer[(Int,Int)]()
      for(i <- 0 until inner_inner_square_size){
        for(j <- 0 until inner_inner_square_size){
          if(!ind_sq_unique.contains(ind_sq(i)(j).abs)){
            ind_sq_unique += ind_sq(i)(j)
            val temp = (i,j)
            ind_sq_unique_address += temp
          }
          if(!cases.contains((ind_sq(i)(j).abs, j))){
            val temp = (ind_sq(i)(j).abs, j)
            cases += temp
            val temp2 = (i,j)
            cases_addr += temp2
          }
        }
      }
      println(ind_sq_unique.zipWithIndex.toList)
      println(ind_sq_unique_address.toList)
      println(cases.toList)
      println(cases_addr.toList)
      def returnMapping2(num: Int, arr: Array[(Int,Int)]):(Int,Int)={//for multiply access values
        var returnval = (0,0)
        for(i <- 0 until arr.length){
          if(num == arr(i)._1){
            returnval = ind_sq_unique_address(i)
          }
        }
        returnval
      }
      val cases_addr_adjusted = cases.map(x=>returnMapping2(x._1.abs,ind_sq_unique.zipWithIndex.toArray))
      println(cases_addr_adjusted.toList)

      def returnMapping3(num: (Int, Int), arr: Array[(Int,Int)]):Int={
        var returnval = 0
        for(i <- 0 until arr.length){
          if(num == arr(i)){
            returnval = i
          }
        }
        returnval
      }
      val new_adj_case_sq = ind_sq.map(x=>x.zipWithIndex.map(y=>(returnMapping3((y._1.abs,y._2), cases.toArray),y._1 < 0)))
      new_adj_case_sq.map(x=>println(x.toList))
      var mult_layer_count1 = 0
      var subadd_layer_count1 = 0
      val CMultLatency = 2
      val CAddLatency = 1
      val initial_Latency = 1
      val multlayerlatency = if(mult_layer_count1>0){1}else{0}
      val subaddlayerlatency = if(subadd_layer_count1 > 0){1}else{0}
      val multadd_latency = if(inner_inner_square_size>1){((Math.log10(inner_inner_square_size)/Math.log10(2)).floor.toInt + (for(l <- 0 until (Math.log10(inner_inner_square_size)/Math.log10(2)).floor.toInt)yield{(inner_inner_square_size/Math.pow(2,l)).floor.toInt % 2}).reduce(_+_)) * (CAddLatency)}else{0}
      val end_latency = 1
      var DFT_latency = initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency + end_latency
      if(r == 2){
        DFT_latency = 1
      }
      println(s"the dft latency: ${DFT_latency}")
      val DFTs_per_stage = width/r
      val number_of_stages = (Math.log10(N)/Math.log10(r)).round.toInt
      val number_of_stages_depth = depth // ideally has to be a divisor of the number of stages
      val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
      var T_L = CMultLatency
      val Twid_latency = (TotalStages+1) * T_L
      val Perm_latency = (N/width)*2
      val Total_Latency = (depth)*T_L + (depth) * DFT_latency + (depth) * Perm_latency + 1 // time for first output val, then wait (N/w) - 1 cycles
      val Latency_exclude_first = (depth)*T_L + (depth) * DFT_latency + (depth) * Perm_latency + 1
      var overrall_latency = 0
      val increment_points = mutable.ArrayBuffer[Int]() // iteration cnt  - 1
      for(i <- 0 until (number_of_stages/number_of_stages_depth)){ // the total number of iterations (number of stages depth must be a divisor of number of stages)
        if(i == 0){
          increment_points += Latency_exclude_first-1 + (N/width)// must be incremented by this point remember the count starts from zero
          overrall_latency += Latency_exclude_first-1 + (N/width) // some cycles to take into account the propagation
        }else{
          increment_points += overrall_latency + Latency_exclude_first - 1 + (N/width)
          overrall_latency += Latency_exclude_first - 1 + (N/width)
        }
      }
      println("increment point check")
      increment_points.map(x=>println(x))
      increment_points += overrall_latency + Perm_latency
      overrall_latency += Perm_latency
      overrall_latency += (N/width)
      println(s"The Overall Latency of Computation: ${overrall_latency}")
      c.io.in_ready.poke(true.B)
      for(i <- 0 until N/width){
        for(j <- 0 until width){
          c.io.in(j).Re.poke(convert_string_to_IEEE_754((i*width + j + 1).toDouble.toString, bw).U)
          c.io.in(j).Im.poke(convert_string_to_IEEE_754(("0.0"), bw).U)
        }
        c.clock.step(1)
      }
      c.io.in_ready.poke(false.B)
      for(i <-0 until overrall_latency - ((N/width)-1)){
        println(s"clock cycle: ${(N/width)+i}")
        println(s"validate: ${c.io.out_validate.peek().litValue}")
        for(j <- 0 until width){
          println(s"Real Output: ${convert_long_to_float(c.io.out(j).Re.peek().litValue, bw)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out(j).Im.peek().litValue, bw)}")
        }
        c.clock.step(1)
        println("-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_")
      }
    }
  }
  def testing_iterations2(N: Int, r: Int, depth: Int, width: Int, bw: Int ): Unit = {
    test(new FFT_SingleRadix_Iterative(N,r,depth,width,bw)){ c=>
      val DFTr_Constants = FFT.DFT_gen(r).map(_.toVector).toVector
      var mult_count = 0
      for(i <- 0 until r-1){
        for(j <- 0 until r-1){
          val n  = DFTr_Constants(i+1)(j+1)
          val c1 = FFT.isReducable(n.re.abs)
          val c2 = FFT.isReducable(n.im.abs)
          if(!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
            mult_count += 1
          }
        }
      }
      val inner_square_size = r-1
      var last_ind = r-1
      var start_ind = 1
      val inner_inner_square_size = inner_square_size/2
      val ind_sq = for(i <- 0 until inner_inner_square_size)yield{
        val row = for(j <- 0 until inner_inner_square_size)yield{
          if(((i+1)*(j+1) % r) > r/2){((i+1)*(j+1) % r) - r}else{((i+1)*(j+1) % r)}
        }
        row
      }
      ind_sq.map(x=>println(x))
      val ind_sq_unique = mutable.ArrayBuffer[Int]()
      val ind_sq_unique_address = mutable.ArrayBuffer[(Int, Int)]()
      val cases = mutable.ArrayBuffer[(Int,Int)]()
      val cases_addr = mutable.ArrayBuffer[(Int,Int)]()
      for(i <- 0 until inner_inner_square_size){
        for(j <- 0 until inner_inner_square_size){
          if(!ind_sq_unique.contains(ind_sq(i)(j).abs)){
            ind_sq_unique += ind_sq(i)(j)
            val temp = (i,j)
            ind_sq_unique_address += temp
          }
          if(!cases.contains((ind_sq(i)(j).abs, j))){
            val temp = (ind_sq(i)(j).abs, j)
            cases += temp
            val temp2 = (i,j)
            cases_addr += temp2
          }
        }
      }
      println(ind_sq_unique.zipWithIndex.toList)
      println(ind_sq_unique_address.toList)
      println(cases.toList)
      println(cases_addr.toList)
      def returnMapping2(num: Int, arr: Array[(Int,Int)]):(Int,Int)={//for multiply access values
        var returnval = (0,0)
        for(i <- 0 until arr.length){
          if(num == arr(i)._1){
            returnval = ind_sq_unique_address(i)
          }
        }
        returnval
      }
      val cases_addr_adjusted = cases.map(x=>returnMapping2(x._1.abs,ind_sq_unique.zipWithIndex.toArray))
      println(cases_addr_adjusted.toList)

      def returnMapping3(num: (Int, Int), arr: Array[(Int,Int)]):Int={
        var returnval = 0
        for(i <- 0 until arr.length){
          if(num == arr(i)){
            returnval = i
          }
        }
        returnval
      }
      val new_adj_case_sq = ind_sq.map(x=>x.zipWithIndex.map(y=>(returnMapping3((y._1.abs,y._2), cases.toArray),y._1 < 0)))
      new_adj_case_sq.map(x=>println(x.toList))
      var mult_layer_count1 = 0
      var cases_no_mult = mutable.ArrayBuffer[(Int, Int)]()
      for(i <- 0 until cases.length) yield {
        if (!FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re)._1 || !FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im)._1) {
          mult_layer_count1 += 1
        } else {
          cases_no_mult += cases_addr_adjusted(i)
        }
      }
      var subadd_layer_count1 = 0
      var cases_no_add = mutable.ArrayBuffer[(Int, Int)]()
      for(i <- 0 until cases.length)yield {
        if (!(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re.abs < 0.00005) && !(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im.abs < 0.00005)) {
          subadd_layer_count1 += 1
        } else {
          cases_no_add += cases_addr_adjusted(i)
        }
      }
      val CMultLatency = 2
      val CAddLatency = 1
      val initial_Latency = 1
      val multlayerlatency = if(mult_layer_count1>0){1}else{0}
      val subaddlayerlatency = if(subadd_layer_count1 > 0){1}else{0}
      val multadd_latency = if(inner_inner_square_size>1){((Math.log10(inner_inner_square_size)/Math.log10(2)).floor.toInt + (for(l <- 0 until (Math.log10(inner_inner_square_size)/Math.log10(2)).floor.toInt)yield{(inner_inner_square_size/Math.pow(2,l)).floor.toInt % 2}).reduce(_+_)) * (CAddLatency)}else{0}
      val end_latency = 1
      var DFT_latency = initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency + end_latency
      val Perm_latency = 1
      val Twid_latency = 2
      if(r==2){
        DFT_latency = 1
      }
      val number_of_stages = (Math.log10(N)/Math.log10(r)).round.toInt
      val latency = (depth*Perm_latency) + (depth*Twid_latency) + (depth*DFT_latency)
      var overrall_latency = 0
      val increment_points = mutable.ArrayBuffer[Int]()
      for(i <- 0 until number_of_stages/depth){
        if(i == 0){
          increment_points += latency
          overrall_latency += latency
        }else{
          increment_points += overrall_latency + latency
          overrall_latency += latency
        }
      }
      increment_points += overrall_latency + Perm_latency
      println("Here are the icnrement points:")
      increment_points.map(x=>println(x))
      overrall_latency += Perm_latency + 1
      println(s"The overrall Latency is: ${overrall_latency}")
      c.io.in_ready.poke(true.B)
      for(i <- 0 until N/width){
        for(j <- 0 until width){
          c.io.in(j).Re.poke(convert_string_to_IEEE_754((i*width + j + 1).toDouble.toString, bw).U)
          c.io.in(j).Im.poke(convert_string_to_IEEE_754(("0.0"), bw).U)
        }
        c.clock.step(1)
      }
      c.io.in_ready.poke(false.B)
      for(i <-0 until overrall_latency - ((N/width)-1)){
        println(s"clock cycle: ${(N/width)+i}")
        println(s"validate: ${c.io.out_validate.peek().litValue}")
        for(j <- 0 until width){
          println(s"Real Output: ${convert_long_to_float(c.io.out(j).Re.peek().litValue, bw)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out(j).Im.peek().litValue, bw)}")
        }
        c.clock.step(1)
        println("-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_")
      }
    }
  }
  def testing_streaming(N: Int, r:Int, w: Int,bw: Int): Unit = {
    test(new FFT_SingleRadix_Streaming(N,r,w,bw)){ c=>
      val DFTs_per_stage = w/r
      val DFT_latency = if(r==2){1}else{4}
      val number_of_stages = (Math.log10(N)/Math.log10(r)).round.toInt
      val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
      var T_L = 2
      val Twid_latency = TotalStages * T_L
      val Perm_latency = (N/w)*2
      val Total_Latency = Twid_latency + (number_of_stages) * DFT_latency + (number_of_stages + 1) * Perm_latency + 1 + (N/w) - 1
      c.io.in_ready.poke(true.B)
      for(j <- 0 until N/w) {
        for (i <- 0 until w) {
          c.io.in(i).Re.poke(convert_string_to_IEEE_754((j*w + i + 1).toDouble.toString, bw).U)
          c.io.in(i).Im.poke(convert_string_to_IEEE_754("0", bw).U)
        }
        c.clock.step(1)
      }
      c.io.in_ready.poke(false.B)
      for(i <- 0 until Total_Latency - ((N/w)-1)){
        println(s"Clock Cycle: ${(N/w) + i}")
        println(s"validate: ${c.io.out_validate.peek().litValue}")
        for(j <- 0 until w){
          println(s"Real Output: ${convert_long_to_float(c.io.out(j).Re.peek().litValue, bw)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out(j).Im.peek().litValue,bw)}")
        }
        c.clock.step(1)
        println("-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_")
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val N = 6
    val r = 3
    val base_r = 3
    val w = 2
    val ptype = 0
    val bw = 32
    val l2c = (log2Ceil((N/w)))
    val pN =6
    val pr = 2
    val pw = 2
    val pwin = 3

    test(new PermutationsWithStreaming_mr(pN,pr,pr,pw,pwin,0,32,((pN/pw) - (pN/pwin)) )){c=>
      for(i <- 0 until (pN/pw)*2 + 1){
        c.io.in_en(i).poke(true.B)
      }
      for(i <- 0 until 30){
        for(j <- 0 until pwin){
          println(s"current input ${((i*pwin + j)%6)}")
          c.io.in(j).Re.poke(convert_string_to_IEEE_754(((i*pwin + j)%6).toDouble.toString, bw).U)
          c.io.in(j).Im.poke(convert_string_to_IEEE_754("0.0", bw).U)
        }
        for(j <- 0 until pw){
          println(s"Real Output: ${convert_long_to_float(c.io.out(j).Re.peek().litValue, bw)}")
          println(s"Imaginary Output: ${convert_long_to_float(c.io.out(j).Im.peek().litValue, bw)}")
        }
        c.clock.step(1)
        println("-_-_-_-_-_-_-_-_-_-_-_-_-_-_-")
      }
    }
  }
}
