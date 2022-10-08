package SWFFT

object ComplexNumbers {
  class complex {
    var re: Double = re // real part
    var im: Double = im // imaginary part
    def print_complex: Unit = { // print the complex number
      println(s"Re: ${re} Im: ${im}")
    }
    def print_mag: Unit = { // print the complex number magnitude
      println(s"Mag: ${complex_mag(this)}")
    }
  }
  type cmplx = complex // creating cmplx type for generating complex numbers
  def cmplx(r:Double, i: Double): complex = {
    var t = new complex
    t.re = r
    t.im = i
    t
  }
  def complex_add(in1: cmplx, in2: cmplx): cmplx = { // complex number addition
    cmplx(in1.re + in2.re, in1.im + in2.im)
  }
  def complex_sub(in1: cmplx, in2: cmplx): cmplx = { // complex number subtraction
    cmplx(in1.re - in2.re, in1.im - in2.im)
  }
  def complex_mult(in1: cmplx, in2: cmplx): cmplx = { // complex number multiplication
    cmplx(in1.re * in2.re - in1.im * in2.im, in1.re * in2.im + in1.im * in2.re)
  }
  def complex_mag(in1: cmplx): Double = { // complex number magnitude computation
    Math.sqrt(in1.re*in1.re + in1.im*in1.im)
  }
  def complex_scalar(in1:cmplx, scal: Double): cmplx = { // multiply complex number by single scalar
    in1.re *= scal
    in1.im *= scal
    in1
  }

  type Wnk = cmplx // creating Wnk type for generating complex exponential used in DFT_NRV
  def Wnk(N: Double, nk: Double): cmplx = {
    var rads = (-1 * (2*Math.PI) / N)*nk
    cmplx(Math.cos(rads), Math.sin(rads))
  }


}
