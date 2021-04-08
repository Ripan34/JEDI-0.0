package test

import context._
import value._
import expression._

object testALU extends App {
  try {
    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Exact(7))))     // 18
    println(alu.execute(Identifier("add"), List(Chars("abc"), Exact(6), Exact(7)))) // abc67
    println(alu.execute(Identifier("less"), List(Chars("abc"), Chars("def"))))  // true
    println(alu.execute(Identifier("div"), List(Exact(100), Exact(2), Exact(2))))     // 25
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("abc"))))  // true
    println(alu.execute(Identifier("more"), List(Exact(5), Exact(6))))  // false
    println(alu.execute(Identifier("not"), List(Boole(false)))) // true

  } catch {
    case e: Exception => println(e)
  }


}