package calculator


sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {        
    namedExpressions.map {x => x._1 -> Signal{eval(x._2(),namedExpressions)}}       
  }
   
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {      
     expr match {
       case Literal(v) => v
       case Plus(a: Expr, b: Expr) => eval(a,references) + eval(b,references)
       case Minus(a: Expr, b: Expr) => eval(a,references) - eval(b,references)
       case Times(a: Expr, b: Expr) => eval(a,references) * eval(b,references)
       case Divide(a: Expr, b: Expr) => eval(a,references) / eval(b,references)
       case Ref(name: String) => {                    
          // IMP!!!!!!!! key of everything we need to evaluate the reference recursively removing the "name" element in the map
          //as cannot reference himself  directly or indirectly within another Expresion CYCLICAL
          eval(getReferenceExpr(name,references),references.filterNot(p => p._1 == name))
           
       }
       case _ => Double.NaN 
     }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
