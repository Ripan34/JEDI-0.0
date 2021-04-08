package value

import expression.Literal

/**
 * addable for Literal
 */
trait Addable extends Literal {
  /**
   * + operator
   */
  def +(other: Value): Addable
}
