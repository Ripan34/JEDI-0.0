package expression
import context.TypeException
import value.{Boole, Environment, Value}

case class Conjunction(cond: List[Expression]) extends SpecialForm{
  if (cond.length < 2) throw new TypeException("Conjunction's input size is less than 2")
  /**
   * execute function
   */
  def execute(env: Environment): Value = {
    var result = true
    var i = 0
    while (result && i < cond.size) {
      val temp = cond(i).execute(env)
      if (!temp.isInstanceOf[Boole]) throw new TypeException("Conjunction operands must be of type Boole")
      result = temp.asInstanceOf[Boole].value
      i += 1
    }
    Boole(result)
  }
}
