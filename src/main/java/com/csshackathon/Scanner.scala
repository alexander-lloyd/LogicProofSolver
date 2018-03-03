package com.csshackathon

class Scanner {


  def main(args: Array[String]): Unit = {
    //Char[] toScan = "~A"

  }

  def toChar(str : String): List[Char]={
    str.toCharArray()
  }

  def scan(inputString:  List[Char]): List[Token] ={
    inputString match {
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
          Var(x) :: scan(xs)
        } else {
          throw Exception
        }
      }
    }
  }

}
