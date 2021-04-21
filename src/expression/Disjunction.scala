package expression
import value.{Boole, Environment, Value}

case class Disjunction(cond: List[Expression]) extends SpecialForm{
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    for(ex <- cond){
      if(ex.isInstanceOf[Boole] && ex.asInstanceOf[Boole].value){
        Boole(true)
      }
    }
    Boole(false)
  }
}
