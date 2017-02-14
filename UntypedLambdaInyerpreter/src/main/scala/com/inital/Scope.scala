package com.inital

/**
  * Created by ajana on 11/15/2016.
  */
object Scope {
  var id = 0
  def nextId = { val i = id; id += 1; i }
  //TOP scope that has no parent.
  val TOP = new Scope(None, Set())
}
class Scope(val parent: Option[Scope], val boundNames: Set[String]) {
  val id = Scope.nextId

  def closestBinding(name: String): Option[Scope] =
    if (boundNames contains name)
      Some(this)
    else
      parent flatMap (_ closestBinding name)
}
class Binder() {
  def apply(term: Expr) = bind(term, Scope.TOP)

  def bind(term: Expr, parent: Scope): Expr = term match {
    case Lambda(arg, body) =>
      val lScope = new Scope(Some(parent), Set(arg.name))
      Lambda(arg.copy(scope = lScope), bind(body, lScope))
    case v @ Var(name, _) =>
      (parent closestBinding name) match {
        case Some(scope) => v.copy(scope = scope)
        case None        => sys.error("Undefined variable: " + name)
      }
    case Apply(fun, arg) =>
      Apply(bind(fun, parent), bind(arg, parent))
  }
}