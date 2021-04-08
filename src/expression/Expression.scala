package expression

import value.{Environment, Value}

trait Expression {
  /**
   * execute function
   */
  def execute(env: Environment): Value
}
