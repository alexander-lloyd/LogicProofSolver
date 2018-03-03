package com.csshackathon

abstract class Token

case class Var(c: Char) extends Token
case class Negation() extends Token
case class Conjunction() extends Token
case class Disjunction() extends Token
case class Implication() extends Token
case class Equivalence() extends Token
case class OpenBracket() extends Token
case class CloseBracket() extends Token
