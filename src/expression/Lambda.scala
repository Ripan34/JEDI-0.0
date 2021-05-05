package expression
import value.{Closure, Environment, Value}

case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    new Closure(params, body, env)
  }
}
