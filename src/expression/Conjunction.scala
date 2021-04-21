package expression
import value.{Boole, Environment, Value}

case class Conjunction(cond: List[Expression]) extends SpecialForm{
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    for(ex <- cond){
      if(ex.isInstanceOf[Boole] && !ex.asInstanceOf[Boole].value){
        Boole(false)
      }
    }
    Boole(true)
  }
}
