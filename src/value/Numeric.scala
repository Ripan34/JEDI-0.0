package value

/**
 * numeric trait for literal
 */
trait Numeric extends Addable {
  def *(other: Value): Numeric

  def -(other: Value): Numeric

  def /(other: Value): Numeric

  def unary_-(): Numeric

}
