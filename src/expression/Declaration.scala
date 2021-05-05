package expression
import value.{Environment, Value, Notification}

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
      env.put(identifier, expression.execute(env))
      Notification.OK
  }
}
