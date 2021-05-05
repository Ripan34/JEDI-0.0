package expression
import value.{Boole, Environment, Value}
import context.{UndefinedException, alu}

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    if(alternative == null) {
      if(condition.execute(env).asInstanceOf[Boole].value)
          consequent.execute(env)
      else
        Boole(false)
    }
    else{
          if(condition.execute(env).asInstanceOf[Boole].value)
            consequent.execute(env)
          else
            alternative.execute(env)
        }

    }

}
