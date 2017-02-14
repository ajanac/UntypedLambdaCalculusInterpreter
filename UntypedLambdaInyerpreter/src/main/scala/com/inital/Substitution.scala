package com.inital

/**
  * Created by ajana on 11/15/2016.
  */
/*apply substitution to the lambda body,  apply it recursively for a Lambda or an Apply,
For Var that is a reference to the same variable ,
 then we just replace it with the replacement Expr.
 */
class Substitution(argV: Var, replacement: Expr) {
  val bindervar = new Binder()

  def apply(term: Expr): Expr = term match {
    case Var(argV.name, argV.scope) => bindervar.bind(replacement, argV.scope.parent.get)
    case Var(_, _)                  => term
    case Apply(fun, arg)            => Apply(apply(fun), apply(arg))
    case Lambda(arg, body)          => Lambda(arg, apply(body))
  }
}
