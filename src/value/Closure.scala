package value

import expression.{Expression, Identifier}

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value{
  def apply(args: List[Value]): Value = {
    val tempEnv: Environment = new Environment(defEnv)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
  }
}
