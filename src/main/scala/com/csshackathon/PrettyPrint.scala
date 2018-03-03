package com.csshackathon


class PrettyPrint {

  def pretty(tree : TokenTree): String = {
    tree match {
      case OpNeg(left) => {
        if (precedence(OpNeg(left)) > precedence(left)) {
          "¬" + "(" + pretty(left) + ")"
        } else{
          "¬" + pretty(left)
        }
      }
      case OpOr(left, right) => {
        if ((precedence(OpOr(left, right)) > precedence(left)) && (precedence(OpOr(left, right)) > precedence(right))){
          "(" + pretty(left) + ")" + "∨" + "(" + pretty(right) + ")"
        } else if (precedence(OpOr(left, right)) > precedence(left)) {
          "(" + pretty(left) + ")" + "∨" + pretty(right)
        } else if (precedence(OpOr(left, right)) > precedence(right)){
          pretty(left) + "∨" + "(" + pretty(right) + ")"
        } else {
          pretty(left) + "∨" + pretty(right)
        }
      }
      case OpAnd(left, right) => {
        if ((precedence(OpAnd(left, right)) > precedence(left)) && (precedence(OpAnd(left, right)) > precedence(right))){
          "(" + pretty(left) + ")" + "∧" + "(" + pretty(right) + ")"
        } else if (precedence(OpAnd(left, right)) > precedence(left)) {
          "(" + pretty(left) + ")" + "∧" + pretty(right)
        } else if (precedence(OpAnd(left, right)) > precedence(right)){
          pretty(left) + "∧" + "(" + pretty(right) + ")"
        } else {
          pretty(left) + "∧" + pretty(right)
        }
      }
      case OpImp(left, right) => {
        if ((precedence(OpImp(left, right)) > precedence(left)) && (precedence(OpImp(left, right)) > precedence(right))){
          "(" + pretty(left) + ")" + "→" + "(" + pretty(right) + ")"
        } else if (precedence(OpImp(left, right)) > precedence(left)) {
          "(" + pretty(left) + ")" + "→" + pretty(right)
        } else if (precedence(OpImp(left, right)) > precedence(right)){
          pretty(left) + "→" + "(" + pretty(right) + ")"
        } else {
          pretty(left) + "→" + pretty(right)
        }
      }
      case OpEqu(left, right) => {
        if ((precedence(OpEqu(left, right)) > precedence(left)) && (precedence(OpEqu(left, right)) > precedence(right))){
          "(" + pretty(left) + ")" + "↔" + "(" + pretty(right) + ")"
        } else if (precedence(OpEqu(left, right)) > precedence(left)) {
          "(" + pretty(left) + ")" + "↔" + pretty(right)
        } else if (precedence(OpEqu(left, right)) > precedence(right)){
          pretty(left) + "↔" + "(" + pretty(right) + ")"
        } else {
          pretty(left) + "↔" + pretty(right)
        }
      }
      case Variable(variable) => {
        variable
      }
    }
  }

  def precedence(tokenTree1: TokenTree): Integer ={
    tokenTree1 match {
      case Variable(_) => {
        return 5
      }
      case OpNeg(_) => {
        return 4
      }
      case OpAnd(_,_) => {
        return 3
      }
      case OpOr(_,_) => {
        return 2
      }
      case OpImp(_,_) => {
        return 1
      }
      case OpEqu(_,_) => {
        return 0
      }
    }
  }
}

object TestPrettyPrint extends App{
  def test1: Unit = {
    var testString: String = "~(AvB^C)"
    var l : List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)
    var pretty = new PrettyPrint()
    println(pretty.pretty(p.parse()))
  }

  def test2: Unit = {
    var testString: String = "(~A)vB^C"
    var l : List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)
    var pretty = new PrettyPrint()
    println(pretty.pretty(p.parse()))
  }


  test1
  test2
}
