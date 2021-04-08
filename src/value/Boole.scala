package value

import context.TypeException
import expression.Literal

/**
 * Boole class
 */
class Boole(val value: Boolean) extends Literal {
  //overloading &&
  def &&(other: Value): Boole = {
    other match{
      case x: Boole => Boole(this.value && x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  }
  //overloading ||
  def ||(other: Value): Boole = {
    other match{
      case x: Boole => Boole(this.value || x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  }
  //overloading !
  def unary_!(): Boole = {
    Boole(!value)
  }

  /**
   * to string
   */
  override def toString: String = value.toString
}
object Boole{
  def apply(value: Boolean) = new Boole(value)
  def FALSE = Boole(false)
  def TRUE = Boole(true)
}
