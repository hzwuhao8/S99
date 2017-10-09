package s99


import scala.language.implicitConversions

object MathOne {
  class IntHelp(n: Int){
    def isPrime(): (Boolean,Option[Int])={
      val x = Math.abs(n)
      val max = Math.sqrt(x).toInt
      val res: Seq[(Int,Boolean)] = for( i <- 2 to max)yield{
         (i, (x % i ) == 0 )
      }
      val res1 = res.forall( _._2 == false)
      if(res1){
        (true, None)
      }else{
        (false,Some( res.filter(_._2 != false).head._1))
      }
    }
  }
  
  
  implicit def int2Help(n: Int): IntHelp = new IntHelp(n)
  
   
  
}