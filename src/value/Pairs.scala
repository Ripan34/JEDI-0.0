package value

case class Pairs(val first: Value, val second: Value) extends Value{
  override def toString: String = ("(" + first.toString + ", " + second.toString + ")")
}
