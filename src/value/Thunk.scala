package value

import expression.Expression

class Thunk(val b: Expression, val en: Environment) extends Closure(Nil, b, en){
  var cache: Value = null

  def apply(): Value ={
    if(cache == null) {
      cache = super.apply(Nil)
    }
      cache
  }
  override def toString = b.execute(en).toString
}
