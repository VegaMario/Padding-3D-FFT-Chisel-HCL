package IEEEConversions
import scala.collection.immutable.Range
import scala.collection.{mutable}
import scala.math.Ordering
import scala.util.control.Breaks.break

object FPConvert { // converting between IEEE 754 and decimal
  // convert IEEE BigInt to BigDecimal representation
  def convert_long_to_float(num: BigInt, bw: Int): BigDecimal = {
    var exponent = 0
    var mantissa = 0
    var zero_1 = BigInt(0)
    if (bw == 16){
      exponent = 5
      mantissa = 10
      zero_1 = BigInt("8000", 16)
    }else if (bw == 32){
      exponent = 8
      mantissa = 23
      zero_1 = BigInt("80000000", 16)
    }else if(bw == 64){
      exponent = 11
      mantissa = 52
      zero_1 = BigInt("8000000000000000", 16)
    }else if(bw == 128){
      exponent = 15
      mantissa = 112
      zero_1 = BigInt("80000000000000000000000000000000", 16)
    }
    if(num == zero_1){
      return BigDecimal(0)
    }
    var n = num
    var list = mutable.ArrayBuffer[String]()
    while(n != 0){
      list += (n % 2).toString
      n = n/2
    }
    while(list.length < bw){
      list += 0.toString
    }
    val sign = list.toList(bw-1)
    val exp = list.slice(mantissa, bw-1).reduce(_+_)
    var sum = binary_string_to_Double(exp, bw)
    var mant = list.slice(0, mantissa).reduce(_+_).reverse
    var new_mant = binary_string_to_Double_Frac(mant, bw)
    new_mant = new_mant * BigDecimal(2).pow((sum-(Math.pow(2, exponent - 1) - 1)).toInt) //Math.pow(2, (sum-(Math.pow(2, exponent - 1) - 1)).toDouble)
    if(sign.toInt == 1)
      new_mant = new_mant * -1
    if(BigInt(num.toString().slice(0, bw-1)) == 0)
      scala.BigDecimal(0.0)
    else
      new_mant
  }

  def binary_string_to_Double(str: String, bw:Int): BigDecimal = {
    var sum:BigDecimal = 0.0
    for(i <- 0 until str.length){
      if(str(i).equals('1')){
        sum += scala.BigDecimal(2).pow(i)
      }
    }
    sum
  }

  def binary_string_to_Double_Frac(str: String, bw: Int): BigDecimal = {
    var sum:BigDecimal = 0.0
    for(i <- 1 to str.length){
      if(str(i-1).equals('1')){
        sum += scala.BigDecimal(2).pow(-1*i)
      }
    }
    sum + 1.0
  }

  // convert any string into the IEEE 754 BigInt representation
  def convert_string_to_IEEE_754 (str: String, bw: Int):BigInt = {
    var exponent = 0
    var mantissa = 0
    var bias = 0
    if (bw == 16){
      exponent = 5
      mantissa = 10
      bias = 15
    }else if (bw == 32){
      exponent = 8
      mantissa = 23
      bias = 127
    }else if(bw == 64){
      exponent = 11
      mantissa = 52
      bias = 1023
    }else if(bw == 128){
      exponent = 15
      mantissa = 112
      bias = 16383
    }
    val max_val = (BigDecimal(2) - BigDecimal(2).pow(-1 * mantissa)) * BigDecimal(2).pow(bias)
    val min_val = BigDecimal(2).pow(-1*(bias-1))
    if (str.equals("0.0") || str.equals('0')){
      return scala.BigInt(0)
    }
    var sign = '0'
    var num = str
    if(str(0).equals('-')){
      sign = '1'
      num = str.slice(1,str.length)
    }
    if(num.contains('E')){
      num = convert_E(num)
    }
    var new_val = BigDecimal(num)
    if( new_val.abs > max_val){
      if(new_val < 0)
        new_val = -1 * max_val
      else
        new_val = max_val
    }else if(new_val.abs < min_val){
      if(new_val < 0)
        new_val = -1 * min_val
      else
        new_val = min_val
    }
    if(new_val.toString().contains('E')){
      num = convert_E(new_val.toString())
    }else{
      num = new_val.toString()
    }
    var part = num.split('.')
    var whole = scala.BigInt(part(0))
    var frac = scala.BigDecimal(("0." + part(1)))
    var list1 = mutable.ArrayBuffer[String]()
    while(whole != 0){
      list1 += (whole % 2).toString
      whole = whole/2
    }
    var whole_str = ""
    if(list1.isEmpty){
      whole_str = "0"
    }else{
      whole_str = list1.reverse.reduce(_+_)
    }

    var new_exp = (whole_str.length - 1 + Math.pow(2, exponent - 1) - 1).toInt
    var list2 = mutable.ArrayBuffer[String]()
    for(i <- 0 until mantissa){
      frac = frac * 2
      if(frac >= 1.0){
        list2 += 1.toString
        frac = frac - 1.0
      }else{
        list2 += 0.toString
      }
    }
    var frac_str = list2.reduce(_+_)
    var exp_adj = 0
    var offset = 0
    var slicefrac = ""
    var fullstr = whole_str+frac_str
    if(BigInt(fullstr) == BigInt(0)){
      while(frac < 1.0){
        frac = frac * 2
        offset += 1
      }
      frac -= 1
      exp_adj = mantissa + offset
      list2 = mutable.ArrayBuffer[String]()
      for(i <- 0 until mantissa){
        frac = frac * 2
        if(frac >= 1.0){
          list2 += 1.toString
          frac = frac - 1.0
        }else{
          list2 += 0.toString
        }
      }
      slicefrac = list2.reduce(_+_)
    }else {
      //var exp_adj = 0
      var i = -1
      do {
        i += 1
        if (fullstr(i) == '0') {
          exp_adj += 1
        } else {
          slicefrac = fullstr.slice(i + 1, fullstr.length)
        }
      } while (fullstr(i) != '1')

      while(slicefrac.length < mantissa) {
        slicefrac += '0'
      }
    }
    new_exp -= exp_adj



    var list5 = mutable.ArrayBuffer[String]()
    while(new_exp != 0){
      list5 += (new_exp % 2).toString
      new_exp /= 2
    }
    while(list5.length < exponent){
      list5 += "0"
    }
    var final_exp = list5.reverse.reduce(_+_)
    var final_str = sign + final_exp + slicefrac.slice(0,mantissa)
    (binary_string_to_Double(final_str.reverse, bw)).toBigInt

  }

  def convert_E(x: String): String = {
    var split = x.split('E')
    var whole_frac = split(0).split('.')
    var full_num = split(0).split('.').reduce(_+_)
    var num = ""
    if(split(1).toInt < 0){
      full_num = full_num.reverse
      for(i <- 0 until split(1).toInt.abs.toInt - 1){
        full_num += '0'
      }
      full_num += ".0"
      full_num = full_num.reverse
      num = full_num
    }else if(split(1).toInt > 0){
      var new_frac = ""
      for(i <- 0 until split(1).toInt){
        if(i < whole_frac(1).length){
          new_frac += whole_frac(1)(i)
        }else{
          new_frac += '0'
        }
      }
      new_frac += '.'
      if(whole_frac(1).length > split(1).toInt){
        for(i <- split(1).toInt until whole_frac(1).length){
          new_frac += whole_frac(1)(i)
        }
      }else{
        new_frac += '0'
      }
      num = whole_frac(0) ++ new_frac
    }
    return num
  }
}
