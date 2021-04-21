package expression
import value.{Environment, Value, Notification}

case class Declaration(identifier: String, expression: Expression) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
      env.put(Identifier(identifier), expression.execute(env))
      Notification().OK
  }
}
