package com.inital

/**
  * Created by ajana on 11/23/2016.
  */
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.RegexParsers
//PackratParsers allow left recursion
class LambdaParser extends StdTokenParsers with PackratParsers{
  type Tokens = StdLexical
  val lexical = new LambdaLexerTerm
  //l is the alias for lambda
  lexical.delimiters ++= Seq("l", ".", "(", ")")
  //Type alias P for Packrat Parsers
  type P[+T] = PackratParser[T]

  lazy val expr: P[Expr]         = lambda | application | variable | parens
  lazy val lambda: P[Lambda]     = "l" ~> variable ~ "." ~ expr ^^
    { case v ~ "." ~ e  => Lambda(v, e) }
  lazy val application: P[Apply] = expr ~ expr ^^
    { case left ~ right => Apply(left, right) }
  lazy val variable: P[Var]      =  ident ^^ Var.apply
  lazy val parens: P[Expr]       = "(" ~> expr <~ ")"



  def parse(source: String): ParseResult[Expr] = {
    val tokens = new lexical.Scanner(source)
    phrase(expr)(tokens)
  }


}
class LambdaLexerTerm extends StdLexical {
  //Clalming l is not a letter
  override def letter = elem("letter", c => c.isLetter && c!='l')
}



object LambdaTerminal{
  val parser = new LambdaParser
  val pretty_printer= new PrettyPrinter
  var bind = new Binder()
  var eval = new EvaluationC(debugVar = false)

  def main(args: Array[String]) = loop()

  def loop() {
    //Infinite loop ask for user input ,parse it and give the result
    while (true) {
      val lambda_expre= readLine("λ> ")
      import parser.{Success, NoSuccess}
      //Pattern matching

      parser.parse(lambda_expre) match {
        case Success(expr, _) => println("Parsed input :"+println(pretty_printer(eval(bind(expr)))))
        case err: NoSuccess   => println("Illiformed Expresson"+err)
      }
    }
  }
}
