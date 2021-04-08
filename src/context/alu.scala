package context

import value._
import expression._

object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "less" => less(args)          // binary
    case "div" => div(args)            // n-ary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary
    // TBC
  }

  private def add(args: List[Value]): Value = {

    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }


  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  /**
   * multiply
   */
private def mul(args: List[Value]): Value = {

  def helper(result: Numeric, unseen: List[Value]): Addable =
    if(unseen == Nil) result
    else helper(result * unseen.head, unseen.tail)

  if(args.size < 2) throw new TypeException("2 or more inputs required by *")
  args(0) match {
    case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
    case _ => throw new TypeException("Inputs to * must be numeric")
  }
}
  /**
   * subtract
   */
  private def sub(args: List[Value]): Value = {

    def helper(result: Numeric, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by -")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to - must be numeric")
    }
  }

  /**
   * Divide
   */
  private def div(args: List[Value]): Value = {

    def helper(result: Numeric, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result / unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by /")
    args(0) match {
      case n: Numeric => helper(args(0).asInstanceOf[Numeric], args.tail )
      case _ => throw new TypeException("Inputs to / must be numeric")
    }
  }

  /**
   * equals (same)
   */
  def same(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by ==")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to == must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] == args(1))
  }

  /**
   * more
   */
  private def more(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by >")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to > must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] > args(1))
  }

  /**
   * unequals
   */
  private def unequals(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by !=")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to != must be orderable")
    !Boole(args(0).asInstanceOf[Ordered[Value]] == args(1))
  }

  /**
   * not
   */
  def not(args: List[Value]): Value = {
    if(args.size != 1) throw new TypeException("1 inputs required by !")
    if(!args(0).isInstanceOf[Boole]) throw new TypeException("Inputs to ! must be Boole")
    !args(0).asInstanceOf[Boole]
  }
}