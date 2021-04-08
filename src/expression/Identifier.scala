package expression
import value.{Environment, Value}
import context.{UndefinedException}

case class Identifier(val name: String) extends Expression{
  override def toString = name
  /**
   * execute function
   */
  override def execute(env: Environment): Value =
    env(this) match{
      case x: Value => x
      case _ => throw new UndefinedException(Identifier(name));
    }

}
