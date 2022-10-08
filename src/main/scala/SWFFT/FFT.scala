package SWFFT
import Permutations._
import ComplexNumbers._
import IEEEConversions.FPConvert.convert_string_to_IEEE_754

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

object FFT {
  // generate arbitrary size DFT_NRV matrix
  def DFT_gen(n: Int):Array[Array[cmplx]] = {
    // initializing the matrix
    var DFT_mtrx = for (i <- 0 until n) yield {
      val row = for (j <- 0 until n) yield {
        cmplx(0,0)
      }
      row.toArray
    }
    var DFT_arr = DFT_mtrx.toArray
    // filling in the matrix with the Wnk terms
    for(i <- 0 until n){
      for(j <- 0 until n){
        DFT_arr(i)(j) = Wnk(n,i*j)
      }
    }
    DFT_arr
  }

  // Perform the computation of the DFT_NRV (matrix * vector) (not the FFT)
  def DFT_compute(DFT: Array[Array[cmplx]], xi: Array[cmplx], N: Int): Array[cmplx] = {
    // initializing output array
    var xk_t = for (i <- 0 until N) yield {
      cmplx(0,0)
    }
    var xk = xk_t.toArray
    // performing the matrix*vector operation
    for(i <- 0 until N){
      for(j <- 0 until N){
        xk(i) = complex_add(xk(i), complex_mult(DFT(i)(j), xi(j)))
      }
    }
    xk
  }

  // single Radix FFT computation
  def FFT_r(N: Int, r: Int, xr: Array[cmplx]): Array[cmplx] = {
    val twiddle_factors = T2(N, r) // generate the twiddle factors
    val l = (Math.log10(N)/Math.log10(r)).round.toInt //calculate the number of stages
    val w = N/r // Calculate the number of DFT computations per stage
    val DFT_r = DFT_gen(r) // generate a DFT_NRV matrix of size r, the radix
    // initialize temporary array
    val temp_t = for (i <- 0 until r) yield {
      cmplx(0, 0)
    }
    var temp = temp_t.toArray
    var Xk = R[cmplx](xr,N, r) // Permute the inputs
    for(i <- 0 until l){ // for each stage
      if(i!=0){ // if not at the first stage
        for(k <- 0 until N){
          Xk(k) = complex_mult( Xk(k), twiddle_factors(i-1)(k)) // apply the twiddle factors
        }
      }
      for(j <- 0 until w){ // for all DFT computations required for the given stage
        for(l <- 0 until r){ // fill in the temporary matrix with r elements from Xk
          temp(l) = Xk(j*r + l)
        }
        temp  = DFT_compute(DFT_r, temp, r) // compute the DFT
        for(l <- 0 until r){ // update the corresponding Xk values with outputs from DFT
          Xk(j*r + l) = temp(l)
        }
      }
      Xk = L[cmplx](Xk,N,r) // Permute Xk
    }
    Xk // return the results of the FFT_r computation
  }

  //useful if you want to visualize the 3D transpose
  def Transpose_3D_example(N:Int, xi: Array[Int]):Array[Int] ={
    var size = N
    var results = xi.clone()
    println(s"-----------------Input---------------------------")
    for(i <- 0 until N){
      for(j <- 0 until N){
        for(k <- 0 until N){
          for(l <- 0 until i){
            print(" ")
          }
          print(s"${results(((N-1-i)*N*N + j + k*N))} ")
        }
        println()
      }
    }
    for(i <- 0 until 3-1){
      size *= N
    }
    for(i <- 0 until 3){
      results = L[Int](results,size,N)
      println(s"-----------------Stage${i} Trans---------------")
      for(i <- 0 until N){
        for(j <- 0 until N){
          for(k <- 0 until N){
            for(l <- 0 until i){
              print(" ")
            }
            print(s"${results(((N-1-i)*N*N + j + k*N))} ")
          }
          println()
        }
      }
      println(s"-----------------------------------------------")
    }
    println(s"-----------------Output--------------------------")

    results
  }

  // Multidimensional FFT/DFT_NRV computation// transpose is done after 1D FFTs
  def MDFFT_PA(d: Int, N:Int, xi: Array[cmplx]):Array[cmplx] ={
    var size = N
    var results = xi.clone()
    for(i <- 0 until d-1){
      size *= N
    }
    for(i <- 0 until d){
      for(j <- 0 until size/N){
        val DFTs = DFT_gen(N)
        val temp = DFT_compute(DFTs,results.slice(j*N,j*N+N),N)
        for(k <- 0 until N){
          results(j*N+k) = temp(k)
        }
      }
      results = L[cmplx](results,size,N) // permutation is done after the FFT in this case// but it can be before
    }
    results
  }

  // Multidimensional FFT/DFT_NRV computation// transpose is done before 1D FFTs
  def MDFFT_PB(d: Int, N:Int, xi: Array[cmplx]):Array[cmplx] ={
    var size = N
    var results = xi.clone()
    for(i <- 0 until d-1){
      size *= N
    }
    for(i <- 0 until d){
      results = L[cmplx](results,size,N) // permutation is done before the the FFT in this case// but it can be after as well
      for(j <- 0 until size/N){
        val DFTs = DFT_gen(N)
        val temp = DFT_compute(DFTs,results.slice(j*N,j*N+N),N)
        for(k <- 0 until N){
          results(j*N+k) = temp(k)
        }
      }
    }
    results
  }

  // this will be used for printing out the golden model
  def MDFFT_goldenmodeluse(d: Int, N:Int, xi: Array[cmplx], FFT_Out_Files:IndexedSeq[(PrintWriter,PrintWriter)], Trans_Out_Files: IndexedSeq[(PrintWriter, PrintWriter)]):Array[cmplx] ={ // In this version the permutation is performed after the FFT stage // assume column inputs
    var size = N // we are assuming that each dimension has length N
    var results = xi.clone() // will store the results
    for(i <- 0 until d-1){ // just getting the total size of the input
      size *= N
    }
    for(i <- 0 until d){
      for(j <- 0 until size/N){ // perform the 1D FFT on all the columns of the dimension
        val DFTs = DFT_gen(N)
        val temp = DFT_compute(DFTs,results.slice(j*N,j*N+N),N) // we take a slice from the set of all inputs // each slice represents a column, which faces toward one dimension
        for(k <- 0 until N){
          results(j*N+k) = temp(k) // we update the old values with the new ones
        }
      }
      for(j <- 0 until size){
        FFT_Out_Files(i)._2.println(results(j).re.toString)
        FFT_Out_Files(i)._2.println(results(j).im.toString)
        FFT_Out_Files(i)._1.println(convert_string_to_IEEE_754(results(j).re.toString, 32).toLong.toHexString)
        FFT_Out_Files(i)._1.println(convert_string_to_IEEE_754(results(j).im.toString, 32).toLong.toHexString)
      }
      results = L[cmplx](results,size,N) // perform the stride permutations// which in this case is the same as a transpose
      for(j <- 0 until size){
        Trans_Out_Files(i)._2.println(results(j).re.toString)
        Trans_Out_Files(i)._2.println(results(j).im.toString)
        Trans_Out_Files(i)._1.println(convert_string_to_IEEE_754(results(j).re.toString, 32).toLong.toHexString)
        Trans_Out_Files(i)._1.println(convert_string_to_IEEE_754(results(j).im.toString, 32).toLong.toHexString)
      }
    }
    results
  }

  // mixed-radix FFT computation
  def FFT_mr(N: Int, nr:Int, ns: Int, r: Int, s: Int, xr:Array[cmplx]): Array[cmplx] = {
    var xk = xr // copy of the inputs array
    val Twid = T2_rs(N,ns,nr) // compute the twiddle factors for the mixed radix case
    xk = L[cmplx](xk,N,nr) // permute the inputs
    // We are splitting DFT_N into nr DFT_ns computations initially
    // afterward, we apply twiddle factors and compute ns DFT_nr computations
    for(i <- 0 until nr){
      var temp = FFT_r(ns,s,xk.slice(i*ns, i*ns+ns)) //compute the FFT_r based on size ns and radix s
      for(j <- 0 until ns){ // update xk values with new values
        xk(i*ns+j) = temp(j)
      }
    }

    xk = L[cmplx](xk,N,ns) // permute the xk

    xk = xk.zip(Twid).map{case (a,b) => complex_mult(a,b)} // apply the twiddle factors
    for(i <- 0 until ns){
      var temp = FFT_r(nr,r,xk.slice(i*nr, i*nr+nr)) // compute the FFT_r based on size nr and radix r
      for(j <- 0 until nr){ // update xk values with new values
        xk(i*nr+j) = temp(j)
      }
    }
    xk = L[cmplx](xk,N,nr) // permute the xk one last time
    xk // return the xk array, now holding the solution
  }

  def getfftstreamedlatency(N: Int, r: Int, w: Int, bw: Int): Int = {
    val DFTr_Constants = FFT.DFT_gen(r).map(_.toVector).toVector
    var mult_count = 0
    for (i <- 0 until r - 1) {
      for (j <- 0 until r - 1) {
        val n = DFTr_Constants(i + 1)(j + 1)
        val c1 = FFT.isReducable(n.re.abs)
        val c2 = FFT.isReducable(n.im.abs)
        if (!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
          mult_count += 1
        }
      }
    }
    val inner_square_size = r - 1
    var last_ind = r - 1
    var start_ind = 1
    val inner_inner_square_size = inner_square_size / 2
    val ind_sq = for (i <- 0 until inner_inner_square_size) yield {
      val row = for (j <- 0 until inner_inner_square_size) yield {
        if (((i + 1) * (j + 1) % r) > r / 2) {
          ((i + 1) * (j + 1) % r) - r
        } else {
          ((i + 1) * (j + 1) % r)
        }
      }
      row
    }
    val ind_sq_unique = mutable.ArrayBuffer[Int]()
    val ind_sq_unique_address = mutable.ArrayBuffer[(Int, Int)]()
    val cases = mutable.ArrayBuffer[(Int, Int)]()
    val cases_addr = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until inner_inner_square_size) {
      for (j <- 0 until inner_inner_square_size) {
        if (!ind_sq_unique.contains(ind_sq(i)(j).abs)) {
          ind_sq_unique += ind_sq(i)(j)
          val temp = (i, j)
          ind_sq_unique_address += temp
        }
        if (!cases.contains((ind_sq(i)(j).abs, j))) {
          val temp = (ind_sq(i)(j).abs, j)
          cases += temp
          val temp2 = (i, j)
          cases_addr += temp2
        }
      }
    }

    def returnMapping2(num: Int, arr: Array[(Int, Int)]): (Int, Int) = { //for multiply access values
      var returnval = (0, 0)
      for (i <- 0 until arr.length) {
        if (num == arr(i)._1) {
          returnval = ind_sq_unique_address(i)
        }
      }
      returnval
    }

    val cases_addr_adjusted = cases.map(x => returnMapping2(x._1.abs, ind_sq_unique.zipWithIndex.toArray))

    def returnMapping3(num: (Int, Int), arr: Array[(Int, Int)]): Int = {
      var returnval = 0
      for (i <- 0 until arr.length) {
        if (num == arr(i)) {
          returnval = i
        }
      }
      returnval
    }

    val new_adj_case_sq = ind_sq.map(x => x.zipWithIndex.map(y => (returnMapping3((y._1.abs, y._2), cases.toArray), y._1 < 0)))
    var mult_layer_count1 = 0
    var cases_no_mult = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until cases.length) yield {
      if (!FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re)._1 || !FFT.isReducable(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im)._1) {
        mult_layer_count1 += 1
      } else {
        cases_no_mult += cases_addr_adjusted(i)
      }
    }
    var subadd_layer_count1 = 0
    var cases_no_add = mutable.ArrayBuffer[(Int, Int)]()
    for (i <- 0 until cases.length) yield {
      if (!(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).re.abs < 0.00005) && !(DFTr_Constants(cases_addr_adjusted(i)._1 + 1)(cases_addr_adjusted(i)._2 + 1).im.abs < 0.00005)) {
        subadd_layer_count1 += 1
      } else {
        cases_no_add += cases_addr_adjusted(i)
      }
    }
    val CMultLatency = 2
    val CAddLatency = 1
    val initial_Latency = 1
    val multlayerlatency = if (mult_layer_count1 > 0) {
      1
    } else {
      0
    }
    val subaddlayerlatency = if (subadd_layer_count1 > 0) {
      1
    } else {
      0
    }
    val multadd_latency = if (inner_inner_square_size > 1) {
      ((Math.log10(inner_inner_square_size) / Math.log10(2)).floor.toInt + (for (l <- 0 until (Math.log10(inner_inner_square_size) / Math.log10(2)).floor.toInt) yield {
        (inner_inner_square_size / Math.pow(2, l)).floor.toInt % 2
      }).reduce(_ + _)) * (CAddLatency)
    } else {
      0
    }
    val end_latency = 1
    var DFT_latency = initial_Latency + multlayerlatency + subaddlayerlatency + multadd_latency + end_latency
    if (r == 2) {
      DFT_latency = 1
    }
    val DFTs_per_stage = w / r
    val number_of_stages = (Math.log10(N) / Math.log10(r)).round.toInt
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    var T_L = CMultLatency
    val Twid_latency = TotalStages * T_L
    val Perm_latency = (N / w) * 2
    val Total_Latency = Twid_latency + (number_of_stages) * DFT_latency + (number_of_stages + 1) * Perm_latency
    Total_Latency
  }

  def getFFTLatency(N:Int, r: Int, w: Int, bw: Int): Int ={
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
    if(r == 2){
      DFT_latency = 1
    }
    val DFTs_per_stage = N/r
    val number_of_stages = (Math.log10(N)/Math.log10(r)).round.toInt
    val TotalStages = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    var T_L = 0
    var mult_cnt_twid = 0
    var subtract_mults = 0
    for(i <- 0 until TotalStages) {
      val twid = Permutations.T2(N, r)(i)
      var mult_count2 = 0
      for (i <- 0 until w){
        val n = twid(i)
        val c1 = FFT.isReducable(n.re.abs)
        val c2 = FFT.isReducable(n.im.abs)
        if (!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
          mult_count2 += 1
        }else if((c1._1 && n.im.abs > 0.005 && n.re.abs > 0.005) || (c2._1 && n.re.abs > 0.005 && n.im.abs > 0.005) ){ // two multipliers eliminated// but adders are maintained
          subtract_mults += 1
        }
      }
      var T_latency = CMultLatency
      mult_cnt_twid += mult_count2
      if (mult_count2 == 0) {
        T_latency = 0
      }
      T_L += T_latency
    }
    var total_mults = if(r==2){0}else{4}
    total_mults += mult_cnt_twid*4 - (subtract_mults*2)
    val twid_adders = mult_cnt_twid * 2
    var total_add = if(r==2){number_of_stages*(N/r)*4}else{number_of_stages*(N/r)*14}
    total_add += twid_adders
    val Twid_latency = (N/w) * CMultLatency
    val Perm_latency = 0
    val Total_Latency = T_L + (number_of_stages) * DFT_latency + (number_of_stages + 1) * Perm_latency
    Total_Latency
  }

  def getFFTLatencymr(N:Int, nr: Int, ns:Int, r: Int, s: Int, w: Int, bw: Int): Int = {
    val FFT_latency1 = getFFTLatency(ns, s, ns, bw)
    val FFT_latency2 = getFFTLatency(nr, r, nr, bw)
    val DFTr_Constants = Permutations.T2_rs(N, ns, nr)
    var mult_count = 0
    val mult_ind = mutable.ArrayBuffer[Int]()
    val mult_needs = for (i <- 0 until w) yield {
      val n = DFTr_Constants(i)
      val c1 = FFT.isReducable(n.re.abs)
      val c2 = FFT.isReducable(n.im.abs)
      if (!((c1._1 && n.im.abs < 0.005) || (c2._1 && n.re.abs < 0.005))) {
        mult_count += 1
        mult_ind += (i)
        (i, 0, false, false, true)
      } else {
        mult_ind += (i)
        if ((c1._1 && n.im.abs < 0.005)) {
          (i, c1._2.abs, n.re < -0.0005, false, false)
        } else {
          (i, c2._2.abs, n.im < -0.0005, true, false)
        }
      }
    }
    val CMultLatency = 2
    var T_latency = CMultLatency
    if (mult_count == 0) {
      T_latency = 0
    }
    val total_latency = FFT_latency1 + FFT_latency2 + T_latency
    total_latency
  }

  def getfftstreamingmrlatency(N:Int, nr: Int, ns: Int, r: Int, s: Int, w: Int, bw: Int): Int = { // streaming single radix fft, still in progress
    val w1 = w // this will be the input and output width
    var w2 = Permutations.getw2(w1, N, nr, ns, s)
    var delay_cycles_stage2 = N / w1 - N / w2 // w2 will be bigger than w1
    // for the first N/w2 clock cycles, the output from the permutation should have a corresponding valid signal,
    // however, for the remaint N/w1-N/w2 clock cycles, the output from the permutation should have an invalid signal
    val CMultLatency = 2
    val T_L = CMultLatency
    var fftlatency1 = 0
    var fftlatency2 = 0
    val perm_latency1 = (N / w1) * 2
    if(w1 < nr && w2 < ns){
      fftlatency1 = getfftstreamedlatency(nr,r,w1,bw)
      fftlatency2 = getfftstreamedlatency(ns,s,w2,bw)
    }else if(w1 < nr && w2 >= ns){
      fftlatency1 = getfftstreamedlatency(nr,r,w1,bw)
      fftlatency2 = getFFTLatency(ns,s,ns,bw)
    }else if(w1 >= nr && w2 < ns){
      fftlatency1 = getFFTLatency(nr,r,nr,bw)
      fftlatency2 = getfftstreamedlatency(ns,s,w2, bw)
    }else if(w1 >= nr && w2 >= ns){
      fftlatency1 = getFFTLatency(nr,r,nr,bw)
      fftlatency2 = getFFTLatency(ns,s,ns,bw)
    }
    val total_latency = perm_latency1*3 + T_L + fftlatency1 + fftlatency2 + 1 // add 1 for output register
    total_latency
  }

  // not-related to FFT, but its used for multiplier/adder hardware reduction (dont know why I have it here)
  def isReducable(num:Double):(Boolean,Int) = {
    val pow = (Math.log10(num)/Math.log10(2)).round
    if((num - Math.pow(2,pow)).abs <= 0.000001)
      (true, pow.toInt)
    else
      (false, 0)
  }
}
