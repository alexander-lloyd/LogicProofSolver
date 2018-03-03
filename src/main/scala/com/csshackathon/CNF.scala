package com.csshackathon

object CNF {

  def toCNF(tokenTree: TokenTree): TokenTree ={
    tokenTree match {
      case OpNeg(OpNeg(left)) => {
        toCNF(left)
      }
      case OpNeg(OpOr(left, right)) => {
        OpAnd(OpNeg(toCNF(left)) , OpNeg(toCNF(right)))
      }
      case OpNeg(OpAnd(left, right)) => {
        OpOr(OpNeg(toCNF(left)),OpNeg(toCNF(right)))
      }
      case OpOr(p, OpAnd(q,r)) => {
        OpAnd(OpOr(toCNF(p), toCNF(q)), OpOr(toCNF(p),toCNF(r)))
      }
      case OpOr(OpAnd(q,r), p) => {
        OpAnd(OpOr(toCNF(p), toCNF(q)), OpOr(toCNF(p),toCNF(r)))
      }
//      case OpAnd(p, OpOr(q,r)) => {
//        OpOr(OpAnd(toCNF(p),toCNF(q)), OpAnd(toCNF(p),toCNF(r)))
//      }
      case OpAnd(p,q) => {
        OpAnd(toCNF(p), toCNF(q))
      }
      case OpNeg(p) => {
        OpNeg(toCNF(p))
      }
      case OpImp(p,q) => {
        OpImp(toCNF(p), toCNF(q))
      }
      case OpEqu(p,q) => {
        OpEqu(toCNF(p), toCNF(q))
      }
      case OpOr(p,q) => {
        OpOr(toCNF(p), toCNF(q))
      }
      case Variable(p) => {
        Variable(p)
      }
    }
  }

  def toSets(tree: TokenTree) : Set[Set[TokenTree]] = {
    tree match {
      case Variable(s) => Set(Set(Variable(s)))
      case OpNeg(wff) => Set(Set(OpNeg(wff)))
      case OpOr(l, r) => {
        // TODO: Not sure...
        Set(toSets(l).head ++ toSets(r).head)
      }
      case OpAnd(l, r) => {
        toSets(l) ++ toSets(r)
      }
    }
  }
}

object TestCNF extends App{
  def test1: Unit = {
    var testString: String = "~(AvB^C)"
    var l : List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)

    var t = CNF.toCNF(p.parse())

    println(PrettyPrint.pretty(t))
  }

  def test2: Unit = {
    var testString: String = "~AvB^C"
    var l : List[Char] = testString.toList

    var tokens = Scanner.scan(l)

    var p = new Parser(tokens)

    println(PrettyPrint.pretty(p.parse()))

    var t = CNF.toCNF(p.parse())

    println(PrettyPrint.pretty(t))
  }


  //test1
  test2
}