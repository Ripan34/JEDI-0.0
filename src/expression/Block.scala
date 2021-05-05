package expression
import value.{Environment, Value}

case class Block(expressions: List[Expression]) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    val temp = new Environment(env)
    for(ele <- expressions){
      temp.put(Identifier(ele.execute(temp).toString),ele.execute(temp))
    }
    expressions(expressions.length-1).execute(temp)
  }
}
