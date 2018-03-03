package com.csshackathon

class ScannerError(message: String) extends Error

object Scanner {

  def main(args: Array[String]): Unit = {
    var str : String = "^A<->()~^v"
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
      case ' ' :: xs => {
        scan(xs)
      }
      case x::xs => {
        if (x.isUpper){
          return Var(x) :: scan(xs)
        }
        throw new ScannerError("Scanner error with: " + x)
      }

    }
  }

}
