package com.csshackathon

class CNF {

  def toCNF(tokenTree: TokenTree): TokenTree ={
    tokenTree match {
      case OpNeg(OpNeg(left)) => {
        toCNF(left)
      }
      case OpNeg(OpOr(left, right)) => {
        OpAnd(OpNeg(left) , OpNeg(right))
      }
      case OpNeg(OpOr(left, right)) => {
        OpOr(OpNeg(left),OpNeg(right))
      }
      case OpOr(p, OpAnd(q,r)) => {
        OpAnd(OpOr(p, q), OpOr(p,r))
      }
      case OpAnd(p, OpOr(q,r)) => {
        OpOr(OpAnd(p,q), OpAnd(p,r))
      }
      //case OpAnd(p,q) =>
    }
  }
}
