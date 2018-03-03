package com.csshackathon

abstract class TokenTree

class Variable(variable: String) extends TokenTree {
  override def toString: String = {
    this.variable
  }
}
class OpNeg(left: TokenTree) extends TokenTree {
  override def toString: String = {
    "~" + this.left
  }
}
class OpAnd(left: TokenTree, right : TokenTree) extends TokenTree {
  override def toString: String = {
    this.left + "^" + this.right
  }
}
class OpOr(left: TokenTree, right : TokenTree) extends TokenTree {
  override def toString: String = {
    this.left + "v" + this.right
  }
}
class OpImp(left: TokenTree, right: TokenTree) extends TokenTree {
  override def toString: String = {
    this.left + "->" + this.right
  }
}
class OpEqu(left: TokenTree, right: TokenTree) extends TokenTree {
  override def toString: String = {
    this.left + "<->" + this.right
  }
}

class ParseError(message: String) extends Error

/**
  * The grammar will be as follows:
  *
  *   expr  -> "~" expr1
  *   expr1 -> expr2 "^" expr2
  *   expr2 -> expr3 "v" expr3
  *   expr3 -> expr4 "->" expr4
  *   expr4 -> primary "<->" primary
  *   primary -> [A-Z]
  *          | "(" expr ")"
  */
class Parser (tokens: List[Token], current : Int = 0) {

  def parse(): TokenTree = {
    val (tree : TokenTree, rest : List[Token]) = expr(this.tokens);
    if (rest != List[Token]()) {
      throw new ParseError("Error, this is left: " + rest.toString)
    }
    else
      tree
  }

  private def expr(tokens: List[Token]): (TokenTree, List[Token]) = {
    tokens match {
      case Negation() :: ts =>
        val (subtree, rest) = expr1(ts)

//        if (rest != List[Token]()) throw new ParseError("Error parsing in expr")
//        else
          (new OpNeg(subtree), List[Token]())

      case ts =>
        val (subtree, rest) = expr1(ts)
//        if (rest != List[Token]()) throw new ParseError("Error parsing in expr")
//        else
          (subtree, rest)
    }
  }

  /*
  Matches ^
   */
  private def expr1(tokens: List[Token]) : (TokenTree, List[Token]) = {
    var (ps_expr : TokenTree, rest : List[Token]) = expr2(tokens)

    rest match {
      case List() => (ps_expr, List())
      case Conjunction() :: ts => {
        val (ps_expr2, rest) = expr2(ts)
        ps_expr = new OpAnd(ps_expr, ps_expr2)
        (ps_expr, rest)
      }
      case _ => {
        (ps_expr, rest)
      }
    }
  }

  /*
  Matches v
   */
  private def expr2(tokens : List[Token]) : (TokenTree, List[Token]) = {
    var (ps_expr : TokenTree, rest : List[Token]) = expr3(tokens)

    rest match {
      case List() => (ps_expr, List())
      case Disjunction() :: ts => {
        val (ps_expr2, rest) = expr3(ts)
        ps_expr = new OpOr(ps_expr, ps_expr2)
        (ps_expr, rest)
      }
      case _ => {
        (ps_expr, rest)
      }
    }
  }

  /*
  Matches ->
 */
  private def expr3(tokens : List[Token]) : (TokenTree, List[Token]) = {
    var (ps_expr : TokenTree, rest : List[Token]) = expr4(tokens)

    rest match {
      case List() => (ps_expr, List())
      case Implication() :: ts => {
        val (ps_expr2, rest) = expr4(ts)
        ps_expr = new OpImp(ps_expr, ps_expr2)
        (ps_expr, rest)
      }
      case _ => {
        (ps_expr, rest)
      }
    }
  }

  /*
  Matches <->
  */
  private def expr4(tokens : List[Token]) : (TokenTree, List[Token]) = {
    var (ps_expr : TokenTree, rest : List[Token]) = primary(tokens)

    rest match {
      case List() => (ps_expr, List())
      case Equivalence() :: ts => {
        val (ps_expr2, rest) = primary(ts)
        ps_expr = new OpImp(ps_expr, ps_expr2)
        (ps_expr, rest)
      }
      case _ => {
        (ps_expr, rest)
      }
    }
  }

  /*
  Variable or Brackets
  */
  private def primary(tokens : List[Token]) : (TokenTree, List[Token]) = {
    tokens match {
      case OpenBracket() :: ts => {
        val (ps_expr, rest) = expr(ts)
        _consume(CloseBracket(), rest)
        (ps_expr, rest)
      }
      case Var(c : Char) :: ts => {
        (new Variable(c.toString), ts)
      }
    }
  }


  // Helper functions

  private def _match(token : Token, tokens: List[Token]) : Boolean = {
    tokens.head == token
  }

  private def _consume(token: Token, tokens: List[Token]) : List[Token] = {
    if (_match(token, tokens)) {
      tokens.tail
    } else {
      throw new ParseError("Error parsing: " + token.toString)
    }
  }
}

object TestParser extends App{
  def test1: Unit = {
    var testString: String = "~(AvB)"
    var l : List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)
    println(p.parse())
  }

  def test2: Unit = {
    var testString: String = "A"
    var l : List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)
    println(p.parse())
  }

  def test3: Unit = {
    var testString: String = "((A^B))"
    var l: List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)
    println(p.parse())
  }

  test1
  test2
  test3
}
