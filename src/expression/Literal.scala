package expression

import value.{Environment, Value}

trait Literal extends Expression with Value {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = this
}
