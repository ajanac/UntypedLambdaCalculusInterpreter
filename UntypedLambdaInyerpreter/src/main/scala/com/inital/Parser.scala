package com.inital

/**
  * Created by ajana on 11/23/2016.
  */
import scala.util.parsing.combinator.PackratParsers
sealed trait Expr
case class Lambda(arg: Var, body: Expr) extends Expr

object Var {
  def apply(name: String): Var = Var(name, Scope.TOP)
}
case class Var(name: String, scope: Scope) extends Expr
case class Apply(fun: Expr, arg: Expr) extends Expr

class PrettyPrinter {

  def apply(expr: Expr): String = expr match {
    case Lambda(arg, body) => p"l$arg.$body"
    case Apply(fun, arg)   => p"$fun $arg"
    case Var(name, scope)  => s"$name${scope.id}"

  }

  implicit class PrettyPrinting(val sc: StringContext) {
    def p(args: Expr*) = sc.s((args map parensIfNeeded):_*)
  }

  def parensIfNeeded(expr: Expr) = expr match {
    case v: Var => apply(v)
    case _         => "(" + apply(expr) + ")"
  }

}
object Parser extends PrettyPrinter{
  def main(args: Array[String]): Unit = {


    val one = Lambda(Var("s"), Lambda(Var("z"), Apply(Var("s"), Var("z"))))
    val pretty = new PrettyPrinter()
    println(pretty(one))
  }


}
