package expression
import value.{Closure, Environment, Notification, Value}
import context.{UndefinedException, alu, flags}

/**
 * Fun call for expression
 * execute all the operands first
 */
case class FunCall(identifier: Identifier, operands: List[Expression]) extends Expression{

  /**
   * execute function
   */
//  override def execute(env: Environment): Value = {
//    var arguments: List[Value] = Nil
//    if (env.contains(identifier)) {
//      if (flags.paramPassing == flags.BY_NAME) {
//        arguments = operands.map(MakeThunk(_).execute(env))
//        Notification.OK
//      }
//      else {
//        if (identifier.execute(env).isInstanceOf[Closure]) {
//          arguments = operands.map(_.execute(env))
//          identifier.execute(env).asInstanceOf[Closure].apply(arguments)
//        }
//        else {
//          arguments = operands.map(_.execute(env))
//          alu.execute(identifier, arguments)
//        }
//      }
//    }
//    else{
//      throw new UndefinedException(Identifier(identifier.toString))
//    }
//  }


  override def execute(env: Environment): Value = {

    val arguments: List[Value] = operands.map(_.execute(env))
    if(env.contains(identifier) && identifier.execute(env).isInstanceOf[Closure]){
        identifier.execute(env).asInstanceOf[Closure].apply(arguments)
    }
    else {
      alu.execute(identifier, arguments)
    }
  }
}
