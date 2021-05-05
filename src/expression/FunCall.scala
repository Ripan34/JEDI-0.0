package expression
import value.{Closure, Environment, Value}
import context.{UndefinedException, alu, flags}

/**
 * Fun call for expression
 * execute all the operands first
 */
case class FunCall(val identifier: Identifier, val operands: List[Expression]) extends Expression{

  /**
   * execute function
   */
  override def execute(env: Environment): Value = {
    var arguments: List[Value] = Nil
    if (env.contains(identifier)) {
      if (flags.paramPassing == flags.BY_NAME) {
        arguments = operands.map(MakeThunk(_).execute(env))
      }
      if(identifier.isInstanceOf[Closure]){
        arguments = operands.map(_.execute(env))
        identifier.execute(env).asInstanceOf[Closure].apply(arguments)
        }
      else{
        arguments = operands.map(_.execute(env))
        alu.execute(identifier, arguments)
      }
    }
    else{
      throw new UndefinedException(Identifier("Unspecified Identifier"))
    }
  }


//  override def execute(env: Environment): Value = {
//
//    val arguments: List[Value] = operands.map(_.execute(env))
//    if(env.contains(identifier) && identifier.execute(env).isInstanceOf[Closure]){
//      identifier.execute(env).asInstanceOf[Closure].apply(arguments)
//    }
//    else {
//      alu.execute(identifier, arguments)
//    }
//  }
}
