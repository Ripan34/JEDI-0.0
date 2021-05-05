package context

import scala.util.parsing.combinator._
import expression._
import value._



class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case con ~ Nil => con
    case con ~ more => Conjunction(con::more)
  }


  def equality: Parser[Expression] = inequality ~ opt("==" ~> inequality) ^^ {
    case ineq ~ None => ineq
    case ineq1 ~ Some(ineq2) => FunCall(Identifier("equals"), List(ineq1, ineq2))
  }

  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case s ~ None => s
    case s ~ Some("<"~s2) =>  FunCall(Identifier("less"), List(s,s2))
    case s ~ Some(">"~s2) =>  FunCall(Identifier("more"), List(s,s2))
    case s ~ Some("!="~s2) =>  FunCall(Identifier("unequals"), List(s,s2))
  }


  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
    case p ~ stuff => parseSums(p, stuff)
  }

  // use tail recursion to imitate left reduce
  // parses a - b + c into add(sub(a, b), c)
  private def parseSums(t: Expression, terms: List[String ~ Expression]): Expression = {
    def combine(exp: Expression, next: String~Expression) =
      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (terms == Nil) t
    else parseSums(combine(t, terms.head), terms.tail)

  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*"|"/") ~ term) ^^ {
    case t ~ stuff => parseProducts(t, stuff)
  }

  private def parseProducts(t: Expression, terms: List[String ~ Expression]): Expression = {
    def combine(exp: Expression, next: String~Expression) =
      next match {
        case "*" ~ p => FunCall(Identifier("mul"), List(exp, p))
        case "/" ~ p => FunCall(Identifier("div"), List(exp, p))
      }
    if (terms == Nil) t
    else parseSums(combine(t, terms.head), terms.tail)

  }

  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

  def literal: Parser[Expression] = boole | real | integer | chars | identifier

  // chars ::= any characters bracketed by quotes
  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
    case characters => Chars(characters.substring(1, characters.length - 1))
  }

  // integer ::= 0|(\+|-)?[1-9][0-9]*
  def integer: Parser[Exact] = """0|(\+|-)?[1-9][0-9]*""".r ^^ {
    case digits => Exact(digits.toInt)
  }

  // real ::= (\+|-)?[0-9]+\.[0-9]+
  def real: Parser[Inexact] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ {
    case digits => Inexact(digits.toDouble)
  }

  // boole ::= true | false
  def boole: Parser[Boole] = """true|false""".r ^^ {
    case booles => Boole(booles.toBoolean)
  }

  // identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case string => Identifier(string.toString)
  }

  // funCall ::= identifier ~ operands
  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case op ~ ops => FunCall(op, ops)
  }

  // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^{
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exps) => e :: exps
    case _ => Nil
  }
}