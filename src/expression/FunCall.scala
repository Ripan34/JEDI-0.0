package expression
import value.{Environment, Value}
import context.alu

/**
 * Fun call for expression
 * execute all the operands first
 */
case class FunCall(val identifier: Identifier, val operands: List[Expression]) extends Expression{

  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    val arguments: List[Value] = operands.map(_.execute(env))
    alu.execute(identifier, arguments)
  }
}
