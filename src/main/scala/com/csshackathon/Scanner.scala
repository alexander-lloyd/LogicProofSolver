package com.csshackathon


object Scanner {

  def main(args: Array[String]): Unit = {
    var str : String = "^A"
    println(scan(str.toList))
  }

  def scan(inputString:  List[Char]): List[Token] ={
    inputString match {
      case List() => {List[Token]()}
      case '~' :: xs => {
        Negation() :: scan(xs)
      }
      case '^' :: xs => {
        Conjunction() :: scan(xs)
      }
      case 'v' :: xs => {
        Disjunction() :: scan(xs)
      }
      case '-':: '>' :: xs => {
        Implication() :: scan(xs)
      }
      case '<' :: '-':: '>' :: xs => {
        Equivalence() :: scan(xs)
      }
      case '(' :: xs => {
        OpenBracket() :: scan(xs)
      }
      case ')' :: xs => {
        CloseBracket()::scan(xs)
      }
      case x::xs => {
        if (x.isUpper){
          return Var(x) :: scan(xs)
        }
        throw new Exception()
      }

    }
  }

}
