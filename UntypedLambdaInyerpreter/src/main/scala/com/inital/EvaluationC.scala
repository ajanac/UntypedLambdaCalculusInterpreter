package com.inital

/**
  * Created by ajana on 11/25/2016.
  */

class EvaluationC(debugVar: Boolean = false) {
  lazy val prettyObj = new PrettyPrinter()

  def apply(termV: Expr): Expr =
    try {
      val termV2 = evalStep(termV)
      if (debugVar)
        println(s"step: ${prettyObj(termV)} → ${prettyObj(termV2)}")
      apply(termV2)
    } catch {
      //lin case none of our patterns above matched
      case _: MatchError => termV
    }

  def evalStep(term: Expr): Expr = term match {
    // substitute arg for all references to argDef in body
    case Apply(Lambda(argDef, body), arg) if isValue(arg) =>
      new Substitution(argDef, arg)(body)
    //  recursively call evaluation again to evaluate arg to a value
    case Apply(fun, arg) if isValue(fun) =>
      Apply(fun, evalStep(arg))
    //left hand side is not yet a value
    case Apply(fun, arg) =>
      Apply(evalStep(fun), arg)
  }

  def isValue(term: Expr): Boolean = term match {
    case _: Lambda => true
    case _: Var    => true
    case _         => false
  }
}