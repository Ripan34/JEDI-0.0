package context

import expression.{Assignment, Expression, FunCall, Identifier, Iteration}

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression
  def assignment: Parser[Expression] = identifier ~ ":=" ~ expression ^^{
    case iden ~ ":=" ~ exp => Assignment(iden, exp)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~> "(" ~> expression ~ ")" ~ expression ^^{
    case cond ~ ")" ~ bod => Iteration(cond, bod)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~> expression <~ "]" ^^{
    case exp => FunCall(Identifier("dereference"), List(exp))
  }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | freeze | funCall | block |  assignment | dereference | literal | "("~>expression<~")"


}
