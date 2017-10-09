package s99

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Prop

/**
 * P31 - P41 
 */

class MathTest extends FunSuite with Checkers {
  import MathOne._
  test("P31 isPrime"){
    assert ( 2.isPrime()._1 )
    assert ( 3.isPrime()._1 )
    check{ ( x: Int)=> 
      val res = x.isPrime()
      if(res._1){
        res._1
      }else{
        x % res._2.get == 0 
      }
    }
  }
  
}