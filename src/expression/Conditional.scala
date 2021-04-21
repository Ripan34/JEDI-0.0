package expression
import value.{Boole, Environment, Value}
import context.{UndefinedException, alu}

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    if(alternative == null) {
      condition match {
        case boole: Boole if boole.value => consequent.execute(env)
        case _ => throw new UndefinedException(Identifier("undefined"))
      }
    }
    else{
      condition match {
        case x => if (x.isInstanceOf[Boole]){
          if(x.asInstanceOf[Boole].value)
            consequent.execute(env)
          else
            alternative.execute(env)
        }
        else
          throw new UndefinedException(Identifier("if expects a Boole value"))
      }
    }
  }
}
