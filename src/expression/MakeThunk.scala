package expression
import value.{Environment, Thunk, Value}

case class MakeThunk(body: Expression) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    new Thunk(body, env)
  }
}
