package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {    
    //IMPORTANT OPERATORS PRECEDENCE with "/" for the desire result
    //SHOULD be like this (2*a()) !!!
    Signal(delta() match {
      case x if(x>0) => Set((-b() + Math.sqrt(delta()))/(2*a()), 
                            (-b() - Math.sqrt(delta()))/(2*a()))
      case 0  => Set(-b()/(2*a()))
      case _ => Set()      
    })
  }
}
