package context

import scala.util.parsing.combinator._
import expression._
import value.{Pairs}
class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^{
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ more) => e::more
    case _ => Nil
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Expression] = "lambda" ~> params ~ expression ^^{
    case p ~ exp => Lambda(p, exp)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"

  def block: Parser[Expression] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^{
    case "{" ~ exp ~ Nil ~ "}" => Block(List(exp))
    case "{" ~ exp ~ list ~ "}" => Block(exp::list)
  }
  // freeze parser
  // freeze ::= "freeze" ~ "(" ~ expression ~ ")" // makes a MakeThunk expression
  def freeze: Parser[Expression] = "freeze" ~> "(" ~> expression <~ ")" ^^{
    case exp => MakeThunk(exp)
  }

  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
}
