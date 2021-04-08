package value
import context.{TypeException}

case class Chars(val value: String) extends Addable with Ordered[Value]{
  /**
   * TO-DO
   * + operator
   */
  override def +(other: Value): Addable = {
    other match{
      case x: Chars => Chars(value + x.value)
      case x: Exact => Chars(value + x.value.toString)
      case x: Inexact => Chars(value + x.value.toString)
      case _ => throw new TypeException("Arguments must be comparable")
    }
  }

  /**
   * sub char
   */
  def subChars(to: Exact, from: Exact): Chars = {
    Chars(this.value.substring(to.value, from.value))
  }
  /**
   * equals
   */
  override def equals(other: Any): Boolean = {
    other match {
      case x: Chars => x.isInstanceOf[Chars] && x.value == this.value
      case x: Inexact => x.isInstanceOf[Inexact] && x.value.toString == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value.toString == this.value
      case _ => false
    }
  }

  /**
   * compare
   */
  override def compare(other: Value): Int =
    other match {
      case x: Chars => this.value.compare(x.value)
      case x: Exact => this.value.compare(x.value.toString)
      case x: Inexact => this.value.compare(x.value.toString)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  /**
   * to String
   */
  override def toString = value

  /**
   * hash code
   */
  override def hashCode(): Int = {
    this.toString.hashCode
  }
  /**
   * size method
   */
  def size(): Exact = {
    Exact(value.size)
  }
}
