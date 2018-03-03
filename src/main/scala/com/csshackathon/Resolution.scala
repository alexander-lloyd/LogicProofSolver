package com.csshackathon

import scala.collection.mutable

object Resolution {

  def resolution(setVals : mutable.Set[mutable.Set[TokenTree]]): Boolean ={
    var applyTo = mutable.Set[TokenTree]()
    for(x <- setVals.toIterator){
      for(y <- setVals.toIterator){
        val resolvent = resolve(x,y)
        if (resolvent.isEmpty){
          return true
        }
        applyTo = resolve(applyTo, resolvent)
        if (applyTo.isEmpty){
          return true
        }
      }
    }
    return false //TODO
  }

  def resolve(set1 : mutable.Set[TokenTree] , set2 : mutable.Set[TokenTree]): mutable.Set[TokenTree] ={
    val unionSet = set1 union set2
    val newUnion = mutable.Set(unionSet.toSeq: _*)
    for(x <- unionSet.toIterator){
      if (unionSet contains OpNeg(x)){
        newUnion.remove(x)
        newUnion.remove(OpNeg(x))
      }
    }
    newUnion
  }


}
