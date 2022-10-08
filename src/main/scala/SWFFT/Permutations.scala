package SWFFT
import ComplexNumbers._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.control.Breaks.break

object Permutations {
  // Perform the L permutation (Stride permutation)
  def L[T:ClassTag]( l: Array[T], n: Int, m: Int): Array[T] = {
    var new_l = l.toArray //create array to store the new result
    var copy_l = l.toArray //create array to hold copy of input array
    var t = l.length / n // determine the number of stages
    var nn = n/m // determine second number for iteration cycles
    for(o <-  0 until t) { // for each stage
      for (i <- 0 until m) {
        for (j <- 0 until nn) {
          // incrementing in strides of m
          new_l(i * nn + j + o * n) = copy_l(j * m + i + o * n) // notice switch between i and j, and nn and m
        }
      }
    }
    new_l
  }

  // Perform the R permutation (bit reversal -- is obtained from repeated use of L)
  def R[T:ClassTag](l: Array[T], n: Int, m:Int): Array[T] = {
    val t_val = (Math.log10(n)/Math.log10(m)).round.toInt // number of stages
    var new_l = l //create copy of input array. Will also hold the final results
    for(i <- 0 until t_val){ // for each stage
      // compute L
      val L_n = Math.pow(m, t_val - i).toInt // L_n decreases each cycle (the size)
      val L_m = m // m stays constant (the stride)
      new_l = L[T](new_l, L_n, L_m) // compute the L permutation and store
    }
    new_l // return the permuted results
  }

  // this function is not used at the moment
  def T(N: Int, r:Int): Array[Array[cmplx]] = {
    val t = ((Math.log10(N) / Math.log10(r)).round - 1).toInt
    val T_mtrx_t = for(i <- 0 until t) yield {
      val T_mini = for(j <- 0 until N) yield{
        cmplx(0,0)
      }
      T_mini
    }
    var t_mtrx = T_mtrx_t.map(_.toArray).toArray
    for(tt <- 0 until t) { // cycle through all sets of groups
      for(l <- 0 until N/(N/Math.pow(r, t-tt).toInt * r)) { // repeat for l times to fill up the twiddle matrix
        for(k <- 0 until (N/Math.pow(r, t-tt).toInt)) { // do for each k index
          for (i <- 0 until r) { // each group has r elements
            t_mtrx(tt)( (N/Math.pow(r, t-tt).toInt)*r*l + r*k + i) = Wnk(N,(i * Math.pow(r, t-tt-1).toInt)*k)
          }
        }
      }
    }
    t_mtrx
  }

  // function used for generating the twiddle factors,
  def T2(N: Int, r:Int): Array[Array[cmplx]] = {
    val t = ((Math.log10(N) / Math.log10(r)) - 1).round.toInt // calculate the total number of sets of twiddle matrices needed
    val T_mtrx_t = for(i <- 0 until t) yield { // initialize array to hold all sets of twiddle matrices
      val T_mini = for(j <- 0 until N) yield{
        cmplx(0,0)
      }
      T_mini
    }
    var t_mtrx = T_mtrx_t.map(_.toArray).toArray
    for(tt <- 0 until t) { // cycle through all sets
      for(k <- 0 until (N/Math.pow(r, t-tt).toInt)) { // do for each k index
        for(l <- 0 until N/(N/Math.pow(r, t-tt).toInt * r)){ // repeat for l times to fill up the twid matrix
          for (i <- 0 until r) { // each group has r elements
            // fill in the twiddle matrices with the Wnk values
            t_mtrx(tt)( (N/(N/Math.pow(r, t-tt).toInt * r))*r*k + r*l + i) = Wnk(N, (i * Math.pow(r, t-tt-1).toInt)*k)
          }
        }
      }
    }
    t_mtrx // return the twiddle matrices
  }

  // this used for computing twiddle factors involved in a mixed radix computation of the FFT
  def T2_rs(N: Int, r:Int, s:Int): Array[cmplx] = {
    val T_mtrx_t = for(j <- 0 until N) yield{ // initialize matrix that will hold the results
        cmplx(0,0)
    }
    var t_mtrx = T_mtrx_t.toArray
    for(k <- 0 until r) { // for each k value
      for (i <- 0 until s) { // there are a total of s terms in equation
        t_mtrx(k*s + i) = Wnk(N, i *k) // fill in the twiddle array with the Wnk values
      }
    }
    t_mtrx // return the twiddle factor array
  }

  // splits the N inputs into N/w groups of w words
  def StreamingGroup[T:ClassTag](xi: Array[T], N: Int, w: Int): Array[Array[T]] = {
    val grouped_xi_t = for(i <- 0 until N/w) yield{ // split into N/w cycles
      val s_xi = for(j <- 0 until w) yield { // w elements each cycle
        xi(i*w + j)
      }
      s_xi
    }
    grouped_xi_t.map(_.toArray).toArray
  }

  // GenerateStreamingMapping is the old design, use v2 instead
  // used for generating the read and store addresses and ports for the permutation streaming datapaths
  // at the momeent it only works for w that is divisor of N
  // N is size
  // w is width
  // r is radix
  // base_r is the radix which r is based on. It could be equal to r if r can not be split anymore
  // t just indicates whether to use the L permutation (0) or R permutation (1)
  def GenerateStreamingMapping(N: Int, w: Int, r: Int, base_r: Int, t:Int):Array[Array[((Int,Int),(Int,Int))]]  = {
    val base_indices = (for(i <- 0 until N)yield{i}).toArray // generate all the indices corresponding to the N elements
    val non_permuted = StreamingGroup[Int](base_indices, N, w) // group indices into streaming groups
    val permuted = StreamingGroup[Int](if(t == 0 || t==2){L[Int](base_indices,N, r)}else{R[Int](base_indices,N,r)},N,w) // group permuted indices into streaming groups
    var unorganized_memory_addr = (for(i <- 0 until N/w) yield{ //find the mappings for each element ((read_address, read_port), (write_address, write_port))
      (for(j <- 0 until w) yield{
        var temp = ((0,0),(0,0))
        for(k <- 0 until N/w){
          for(l <- 0 until w){
            if(non_permuted(i)(j)==permuted(k)(l)){
              temp = ((i,j), (k,l))
            }
          }
        }
        temp
      }).toArray
    }).toArray
    var organized_into_unique_mem_stores = (for(i <- 0 until N/w) yield{(for(j <- 0 until w) yield{((0,0),(0,0))}).toArray}).toArray
    var index = 0
    for(i <- 0 until N/w){
      for(j <- 0 until N/w){
        for(k <- 0 until w){
          if(i == unorganized_memory_addr(j)(k)._2._1){
            // organize elements into a form that has unique read_ports for each group of w words
            organized_into_unique_mem_stores(i)(index) = unorganized_memory_addr(j)(k)
            index = (index + 1) % w
          }
        }
      }
    }
    if(t == 2) { // I think the t == 2 case might be the better solution for the permutations
      val horizontal_length = unorganized_memory_addr(0).length
      val vertical_length = unorganized_memory_addr.length
      for(l <- 0 until vertical_length-1) {
        val used_indices = mutable.ArrayBuffer[Int]()
        used_indices += unorganized_memory_addr(l)(0)._2._2
        val m = mutable.ArrayBuffer[Array[Array[((Int, Int), (Int, Int))]]]()
        val m2 = mutable.ArrayBuffer[Array[((Int, Int), (Int, Int))]]()
        val ta = (for (i <- l until vertical_length) yield {
          unorganized_memory_addr(i)(0)
        }).toArray
        m2 += ta
        m += m2.toArray
        m2.clear()
        for (i <- 1 until horizontal_length) {
          var temp = (for (j <- l until vertical_length) yield {
            unorganized_memory_addr(j)(i)
          }).toArray
          m2 += temp
          var cnt = 0 + l
          var flag = false
          while (used_indices.contains(unorganized_memory_addr(cnt)(i)._2._2) && !flag) {
            temp = circular_shift[((Int, Int), (Int, Int))](temp, 1)
            if(cnt == vertical_length-1){
              flag = true
              val temp_missing = mutable.ArrayBuffer[Int]()
              var index_switch = 0
              //              circular_shift[((Int, Int), (Int, Int))](temp, 1)
              for(v <- 0 until horizontal_length){
                if(!used_indices.contains(v)){
                  temp_missing += v // this finds the missing index
                }
              }
              var keep_looping = true

              // want to make this into a function
              var temp2 = temp.clone()
              while(keep_looping) {
                for (v <- 0 until used_indices.length) {
                  if (used_indices(v) == temp2(0)._2._2 && v != i) {
                    index_switch = v
                  }
                }

                temp2 = (for (j <- l until vertical_length) yield {
                  m(index_switch)(0)(j - l) //updates column of values
                }).toArray

                var cnt2 = 0 + l
                var flag2 = false
                while (temp2(0)._2._2 != temp_missing(0) && !flag2) { // now checking to see if the missing number is here
                  if (cnt2 == vertical_length - 1) { // if we have not found it
                    flag2 = true
                  } else {
                    temp2 = circular_shift[((Int, Int), (Int, Int))](temp2, 1) // otherwise keep looking
                    cnt2 += 1
                  }
                }
                for (j <- l until vertical_length) {
                  m(index_switch)(0)(j - l) = temp2(j - l) //update the column with what u have
                }
                keep_looping = flag2
              }
            }
            m2.clear()
            m2 += temp
            if(cnt == vertical_length-1) {
              cnt = cnt
            }else{
              cnt += 1
            }
          }
          if(flag){
            val temp_missing = mutable.ArrayBuffer[Int]()
            for(v <- 0 until horizontal_length){
              if(!used_indices.contains(v)){
                temp_missing += v
              }
            }
            used_indices += temp_missing(0)
            m += m2.toArray
            m2.clear()
          }else {
            m += m2.toArray
            m2.clear()
            used_indices += unorganized_memory_addr(cnt)(i)._2._2
          }
        }
        val organ = (for (i <- l until vertical_length) yield {
          (for (j <- 0 until horizontal_length) yield {
            m(j)(0)(i-l)
          }).toArray
        }).toArray
        for (i <- l until vertical_length) {
          for (j <- 0 until horizontal_length) {
            unorganized_memory_addr(i)(j) = organ(i-l)(j)
          }
        }


      }
      unorganized_memory_addr
    }else {
      var vertical_group_size = N / (w * r) // Vertical groups are considered to be ones that have the same read_port accross the vertical elements (see output)
      var horizontal_group_size = w / r // the w horizontal terms can be divided into groups of equivalent sizes.
      var rxr_group_size = r // horizontal group count = vertical group count
      if (w != r && r != N / w) {
        var pow = r
        var is_pow = false
        while (pow != N) {
          pow *= r
          if (pow == w) // trying to determine if w is a power of r
            is_pow = true
        }
        if (!is_pow) { // if w is not a power of r
          var increment_amount = (Math.log10(r) / Math.log10(base_r)).round.toInt
          var starting_point = N
          var power_set = 0
          var stage = 0
          var multiplier = 1
          var count = 0
          var stop_cond = false
          while (!stop_cond) {
            if (starting_point == w) {
              power_set = stage
              if (power_set == 0) {
                multiplier = 1
              } else {
                multiplier = Math.pow(base_r, count - 1).toInt
              }
              stop_cond = true
            } else {
              starting_point /= base_r
            }
            if (count == increment_amount - 1) {
              if (stage == 0) {
                stage = 1
              } else {
                stage += increment_amount
              }
            }
            count = (count + 1) % increment_amount
          }
          vertical_group_size = Math.pow(base_r, power_set).toInt * multiplier
          rxr_group_size = (N / w) / (vertical_group_size)
          horizontal_group_size = w / (rxr_group_size)
        } else {
          vertical_group_size = N / (w * r * r)
          horizontal_group_size = w / (r * r)
          rxr_group_size = r * r
        }
      }
      var arranged_into_4D = (for (i <- 0 until rxr_group_size) yield {
        val t1 = (for (j <- 0 until rxr_group_size) yield {
          val t2 = (for (k <- 0 until vertical_group_size) yield {
            val t3 = (for (l <- 0 until horizontal_group_size) yield {
              organized_into_unique_mem_stores(j * vertical_group_size + k)(i * horizontal_group_size + l)
            }).toArray
            t3
          }).toArray
          t2
        }).toArray
        t1
      }).toArray
      for (i <- 0 until rxr_group_size) {
        arranged_into_4D(i) = circular_shift[Array[Array[((Int, Int), (Int, Int))]]](arranged_into_4D(i), i)
      }
      for (i <- 0 until rxr_group_size) {
        for (j <- 0 until rxr_group_size) {
          for (k <- 0 until vertical_group_size) {
            for (l <- 0 until horizontal_group_size) {
              organized_into_unique_mem_stores(j * vertical_group_size + k)(i * horizontal_group_size + l) = arranged_into_4D(i)(j)(k)(l)
            }
          }
        }
      }
      organized_into_unique_mem_stores
    }
  }

  // circular shift on any list
  def circular_shift[T:ClassTag](xi: Array[T], amnt: Int): Array[T] = {
    val cpy = xi // hold first element
    for(i <- 0 until amnt){
      val tmp = xi(0)
      for(j <- 0 until xi.length - 1){
        cpy(j) = cpy(j+1) // shift all other elements up
      }
      cpy(xi.length-1) = tmp // place first element at the end
    }
    cpy
  }

  def Mapping_Sort(xi: Array[Array[((Int,Int),(Int,Int))]], t: Int) : Array[Array[((Int,Int),(Int,Int))]] = {
    var temp = 0
    var temp2 = 0
    var temp3 = ((0,0),(0,0))
    for(k <- 0 until xi.length) {
      for (i <- 0 until xi(0).length-1) {
        if(t == 1){
          temp = xi(k)(i)._2._2
        }else{
          temp = xi(k)(i)._1._2
        }
        for (j <- i + 1 until xi(0).length) {
          if(t == 1){
            temp2 = xi(k)(j)._2._2
          }else{
            temp2 = xi(k)(j)._1._2
          }
          if (temp2 < temp) {
            temp3 = xi(k)(j)
            xi(k)(j) = xi(k)(i)
            temp = temp2
            xi(k)(i) = temp3
          }
        }
      }
    }
    xi
  }

  def generate_streaming_possibilities(N: Int, r: Int): Array[Int] ={
    val lists = mutable.ArrayBuffer[Int]()
    for(i <- r until N by r){
      if((N.toDouble/i.toDouble).isWhole){
        lists += i
      }
    }
    if(r == N){
      lists += r
    }
    lists.toArray
  }

  def getw2(w1: Int, N: Int, nr: Int, ns: Int, s: Int): Int = {
    var w2 = 0
    if(w1 == 2){
      w2 = 3
    }else{
      if(w1 < nr) {
        if(ns != s) {
          val generated_mappings = Permutations.generate_streaming_possibilities(ns, s)
          var counter = 0
          var flag = false
          while (generated_mappings(counter) < w1 && !flag) {
            if(counter == generated_mappings.length-1){
              flag = true
            }else {
              counter += 1
            }
          }
          if(flag){
            w2 = ns
          }else{
            w2 = generated_mappings(counter)
          }
        }else{
          val generated_mappings = Permutations.generate_streaming_possibilities(N, s)
          var counter = 0
          while (generated_mappings(counter) < w1) {
            counter += 1
          }
          w2 = generated_mappings(counter)
        }
      }else{
        val generated_mappings2 = Permutations.generate_streaming_possibilities(ns,s)
        var flag = false
        for(i <- 0 until generated_mappings2.length){
          if(generated_mappings2(i) > w1 && (N.toDouble/generated_mappings2(i).toDouble).isWhole){
            flag = true
          }
        }
        if(!flag) {
          val generated_mappings = Permutations.generate_streaming_possibilities(N, ns)
          var counter = 0
          while (generated_mappings(counter) < w1) {
            counter += 1
          }
          w2 = generated_mappings(counter)
        }else{
          val generated_mappings = Permutations.generate_streaming_possibilities(ns, s)
          var counter = 0
          while (generated_mappings(counter) < w1 || !(N.toDouble/generated_mappings(counter).toDouble).isWhole) {
            counter += 1
          }
          w2 = generated_mappings(counter)
        }
      }
    }
    w2
  }

  // more general way for generating streaming mapping
  def GenerateStreamingMappingV2(N: Int, w: Int, r: Int, base_r: Int, t:Int) = { // This is even more general than the first function
    val base_indices = (for(i <- 0 until N)yield{i}).toArray // generate all the indices corresponding to the N elements
    val non_permuted = StreamingGroup[Int](base_indices, N, w) // group indices into streaming groups
    val permuted = StreamingGroup[Int](if(t == 0){L[Int](base_indices,N, r)}else{R[Int](base_indices,N,r)},N,w) // group permuted indices into streaming groups
    var unorganized_memory_addr = (for(i <- 0 until N/w) yield{ //find the mappings for each element ((read_address, read_port), (write_address, write_port))
      (for(j <- 0 until w) yield{
        var temp = ((0,0),(0,0))
        for(k <- 0 until N/w){
          for(l <- 0 until w){
            if(non_permuted(i)(j)==permuted(k)(l)){
              temp = ((i,j), (k,l))
            }
          }
        }
        temp
      }).toArray
    }).toArray
    //before sorting
    val cpy = unorganized_memory_addr.clone()
    val horizontal_length = w
    val vertical_length = N/w
    for(l <- 0 until vertical_length-1) { // we are checking every row to make sure that there are no repeated M1 ports
      val used_indices = mutable.ArrayBuffer[Int]() // keeps track of the
      used_indices += unorganized_memory_addr(l)(0)._2._2 // we preload the first element of each row into the buffer
      val all_columns = mutable.ArrayBuffer[Array[Array[((Int, Int), (Int, Int))]]]() // we use this to hold all sorted columns
      val single_column = mutable.ArrayBuffer[Array[((Int, Int), (Int, Int))]]() // we use this to hold one column
      val get_first_column = (for (i <- l until vertical_length) yield {
        unorganized_memory_addr(i)(0)
      }).toArray
      single_column += get_first_column // we are preloading the first column
      all_columns += single_column.toArray
      single_column.clear() // we clear the buffer to get the next column
      for (i <- 1 until horizontal_length) { //we are now checking all the elements within the same row
        var temp = (for (j <- l until vertical_length) yield {
          unorganized_memory_addr(j)(i) // organizing them by columns, notice that the column sizes get smaller every iteration of l
        }).toArray
        single_column += temp
        var cnt = 0 + l // counter used for cycling through elements
        var flag = false // lets us know if no new numbers are found
        while (used_indices.contains(unorganized_memory_addr(cnt)(i)._2._2) && !flag) { // trying to search for new numbers not previously used
          temp = circular_shift[((Int, Int), (Int, Int))](temp, 1) // it a applies a circular style shift to the rows
          if(cnt == vertical_length-1){ // however, if we have reached the end of the column length and we still have not found a new number
            var temp2 = temp.clone()
            flag = true // the flag means no new numbers have been found
            val temp_missing = mutable.ArrayBuffer[Int]() // find out which numbers are missing
            var index_switch = 0 // switch the index position in which we are looking
            for(v <- 0 until horizontal_length){
              if(!used_indices.contains(v)){
                temp_missing += v // this finds the missing indices // even if there are many, we only focus on the first one we find
              }
            }
            var keep_looping = true // we will be repeating a searching process until we find no repeating numbers
            while(keep_looping) {
              for (v <- 0 until used_indices.length) { // first find in which spot the repeated number first appeared
                if (used_indices(v) == temp2(0)._2._2 && v != i) {
                  index_switch = v
                }
              }
              temp2 = (for (j <- l until vertical_length) yield { // for the row index in which the number first appeared, get the column pertaining to the row index
                all_columns(index_switch)(0)(j - l) //updates column of values
              }).toArray
              var cnt2 = 0 + l // similar counter procedure is trying to determine if the missing number can be found within the column corresponding to the row index
              var flag2 = false // flag2 indicates if the missing number was not found and we need to search more
              while (temp2(0)._2._2 != temp_missing(0) && !flag2) { // now checking to see if the missing number is here
                if (cnt2 == vertical_length - 1) { // if we are not able to find, we will have to search elsewhere
                  flag2 = true
                } else {
                  temp2 = circular_shift[((Int, Int), (Int, Int))](temp2, 1) // otherwise we keep circular shifting the column to search for the missing number
                  cnt2 += 1
                }
              }
              for (j <- l until vertical_length) {
                all_columns(index_switch)(0)(j - l) = temp2(j - l) // we will update the column we selected regardless if we found the missing number.
              }
              keep_looping = flag2 // the flag2 tells us if we need to keep looping
            } // loop back or stop
          }
          single_column.clear()
          single_column += temp // we should now have
          if(cnt == vertical_length-1) {
            cnt = cnt
          }else{
            cnt += 1
          }
        }
        if(flag){
          val temp_missing = mutable.ArrayBuffer[Int]()
          for(v <- 0 until horizontal_length){
            if(!used_indices.contains(v)){
              temp_missing += v
            }
          }
          used_indices += temp_missing(0)
        }else {
          used_indices += unorganized_memory_addr(cnt)(i)._2._2
        }
        all_columns += single_column.toArray
        single_column.clear()
      }
      val organ = (for (i <- l until vertical_length) yield {
        (for (j <- 0 until horizontal_length) yield {
          all_columns(j)(0)(i-l)
        }).toArray
      }).toArray
      for (i <- l until vertical_length) {
        for (j <- 0 until horizontal_length) {
          unorganized_memory_addr(i)(j) = organ(i-l)(j)
        }
      }
    }
    val organized_memory_addr = unorganized_memory_addr
    organized_memory_addr
  }
}
