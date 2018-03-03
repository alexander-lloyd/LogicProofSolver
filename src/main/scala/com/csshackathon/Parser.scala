package com.csshackathon

abstract class TokenTree

class Variable(variable: String) extends TokenTree
class OpNeg(left: TokenTree) extends TokenTree
class OpAnd(left: TokenTree, right : TokenTree) extends TokenTree
class OpOr(left: TokenTree, right : TokenTree) extends TokenTree
class OpImp(left: TokenTree, right: TokenTree) extends TokenTree
class OpEqu(left: TokenTree, right: TokenTree) extends TokenTree


/**
  * The grammar will be as follows:
  *
  *   expr  -> "~" expr1
  *   expr1 -> expr2 "^" expr2
  *   expr2 -> expr3 "v" expr3
  *   expr3 -> expr4 "->" expr4
  *   expr4 -> expr5 "<->" expr5
  *   expr5 -> [A-Z]
  *          | expr
  */
class Parser {

  def parse(tokens: List[Token]): TokenTree = {
    expr(tokens)
  }

  def expr(tokens: List[Token]): TokenTree = {
    tokens match {
      case Negation() :: ts =>
        var subtree = expr1(ts)
        return OpNeg(subtree)
    }
  }

  def expr1(tokens: List[Token]) : TokenTree = {

  }

}

trait TestParser extends App{
  var p = new Parser()
  p.pars

}
