package expression
import context.TypeException
import value.{Boole, Environment, Notification, Value}

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    if (condition.execute(env).isInstanceOf[Boole]) {
      while(condition.execute(env).asInstanceOf[Boole].value){
        body.execute(env)
      }
      Notification.DONE
    }
    else{
      println("iter")
      throw new TypeException
    }
  }
}
