package expression
import context.UndefinedException
import value.{Environment, Notification, Value, Variable}

case class Assignment(var identifier: Identifier, var update: Expression) extends SpecialForm{
  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    if(env.contains(identifier)){
      env(identifier) = Variable(update.execute(env))
      println(env(identifier) + " assignm")
      Notification.DONE
    }
    else{
      throw new UndefinedException(Identifier("Unspecified Identifier"))
    }
  }
}
