package SWFFT
import Permutations._
import ChiselFFT.FFTSRDesigns._
import better.files.StringInterpolations
import chisel3.getVerilogString
import chisel3.tester.RawTester.test
import chisel3._
import chisel3.tester._
import IEEEConversions.FPConvert._
import FloatingPointDesigns.FPArithmetic._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random
import java.io.{File, PrintWriter}
import ChiselFFT.PaddingDesigns._

object FFTMain {
  def main(args: Array[String]): Unit = {
    //This is for testing the software based FFT Designs
    //testing the single radix FFT
    val N = 4 // the size of FFT
    val r = 2 // the radix of FFT
    val xi = (1 to N).toArray.map(x=>ComplexNumbers.cmplx(x,0))
//    val results_sr = FFT.FFT_r(N,r,xi)

    //testing the mixed-radix FFT
    val N_mr = 6 // the size of FFT
    val r_mr = 2 // the radix r
    val s_mr = 3 // the radix s
    val nr_mr = 2 // the r^k
    val ns_mr = 3 // the s^l
    val xi_mr = (1 to N_mr).toArray.map(x=>ComplexNumbers.cmplx(x,0))
//    val results_mr = FFT.FFT_mr(N_mr,nr_mr,ns_mr,r_mr,s_mr,xi_mr)

    //testing the multi-dimensional FFT
    val d_md = 3 // the dimension
    val N_md = 2 // the size of each dimension
    val xi_md = (1 to Math.pow(N_md,d_md).round.toInt).toArray.map(x=>ComplexNumbers.cmplx(x,0))
//    val results_md = FFT.MDFFT_PB(d_md,N_md, xi_md)

//    val radius = 21
//    val radii_of_slice = for(i <- 0 until 21) yield{
//      val x=i
//      Math.sqrt((radius*radius) - (x*x)).toInt
//    }
//    println(s"Here the radii of each slice: ${radii_of_slice}")
//    var element_slices = mutable.ArrayBuffer[Array[Int]]()
//    val elements_per_slice = mutable.ArrayBuffer[Int]()
//    val area_of_slices = for(i <- 0 until 21) yield{
//      var area = 0
//      for(j <- 0 until radii_of_slice(i)){
//        val x = j
//        val new_d = (Math.sqrt((radii_of_slice(i) * radii_of_slice(i)) - (x * x)).toInt) * 2
//        println(s"new_d: ${new_d}")
//        elements_per_slice += new_d
//        if(j == 0)
//          area += new_d
//        else
//          area += new_d*2
//      }
//      element_slices += elements_per_slice.toArray.reverse
//      elements_per_slice.clear()
//      println("end of iter\n")
//      if(i==0)
//        area
//      else
//        area * 2
//    }
//    println(s"Here are the areas of each slice (non-centered ones are doubled): ${area_of_slices}")
//    val total_volume = area_of_slices.reduce(_+_)
//    println(s"The total discrete volume of the cube is: ${total_volume}")
//    element_slices = element_slices.reverse
//    element_slices.map(x=>println(x.toList))
//    val sphere = mutable.ArrayBuffer[Array[Array[Int]]]()
//    val slice = mutable.ArrayBuffer[Array[Int]]()
//    val row = mutable.ArrayBuffer[Int]()
//    for(i <- 0 until element_slices.length){
//      for(j <- 0 until element_slices(i).length){
//        for(k <- 0 until element_slices(i)(j)){
//          row += 1
//        }
//        slice += row.toArray
//        row.clear()
//      }
//      for (j <- 1 until element_slices(i).length) {
//        for (k <- 0 until element_slices(i).reverse(j)) {
//          row += 1
//        }
//        slice += row.toArray
//        row.clear()
//      }
//      sphere += slice.toArray
//      slice.clear()
//    }
//    for(i <- 1 until element_slices.length){
//      for(j <- 0 until element_slices.reverse(i).length){
//        for(k <- 0 until element_slices.reverse(i)(j)){
//          row += 1
//        }
//        slice += row.toArray
//        row.clear()
//      }
//      for(j <- 1 until element_slices.reverse(i).length){
//        for(k <- 0 until element_slices.reverse(i).reverse(j)){
//          row += 1
//        }
//        slice += row.toArray
//        row.clear()
//      }
//      sphere += slice.toArray
//      slice.clear()
//    }
//    println("\nPrinting out the sphere:\n")
//    for(i <- 0 until sphere.length){
//      for(j <- 0 until sphere(i).length){
//        println(sphere(i)(j).toList)
//      }
//      println("end of slice\n")
//    }
//
//    val padded_sphere = mutable.ArrayBuffer[Array[Array[Int]]]()
//    val padded_slice = mutable.ArrayBuffer[Array[Int]]()
//    val padded_row = mutable.ArrayBuffer[Int]()
//    for(i <- 0 until sphere.length){
//      for(k <- 0 until sphere(i).length)yield{
//        val pad_amnt = (42 - sphere(i)(k).length)/2
//        for(j <- 0 until pad_amnt){
//          padded_row += 0
//        }
//        for(j <- 0 until sphere(i)(k).length){
//          padded_row += sphere(i)(k)(j)
//        }
//        for (j <- 0 until pad_amnt) {
//          padded_row += 0
//        }
//        padded_slice += padded_row.toArray
//        padded_row.clear()
//      }
//      padded_sphere += padded_slice.toArray
//      padded_slice.clear()
//    }
//    println("\nPrinting out the padded sphere:\n")
//    for (i <- 0 until padded_sphere.length) {
//      for (j <- 0 until padded_sphere(i).length) {
//        for(k <- 0 until padded_sphere(i)(j).length){
//          if(padded_sphere(i)(j)(k) != 0)
//            print("1\t")
//          else
//            print('\t')
//        }
//        println()
//      }
//      println("end of padded slice\n")
//    }

//    test(new streaming_padding(Array(42),42,2,96,32,Array(27),Array(21),Array(34))){c=>
//      c.io.in_start.poke(true.B)
//      for(i <- 0 until 96/2){
//        for(j <- 0 until 2){
//          println((i*2+j+1))
//          c.io.in(j).Re.poke(convert_string_to_IEEE_754((i*2+j + 1).toDouble.toString, 32).U)
//          c.io.in(j).Im.poke(convert_string_to_IEEE_754("0.0", 32).U)
//        }
//        c.clock.step(1)
//        println(s"clock cycle: ${i+1}")
//        for(j <-0 until 2){
//          println(s"Output ${i*2+j}: ${convert_long_to_float(c.io.out(j).Re.peek().litValue, 32)}")
//        }
//      }
//    }
  }
}
