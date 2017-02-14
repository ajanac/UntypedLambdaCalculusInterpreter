package com.initial.test

/**
  * Created by ajana on 11/25/2016.
  */
import org.scalatest.FlatSpec
import com.inital.LambdaParser
import com.inital.PrettyPrinter
import com.inital.EvaluationC
import com.inital.Binder

import com.inital.Expr
class LambdaParserTestOne extends FlatSpec  {
  "A Parser" should "parse the lambda expression in the legitimate form" in {
    val parserTest = new LambdaParser
    var bind = new Binder()
    var eval = new EvaluationC(debugVar = false)

    val pretty_printer_test= new PrettyPrinter

   // assert(pretty_printer_test(eval(bind("lx.xy"))) === "????")
  }

  it should "throw  Illiformed Expresson" in {
    val parserErrorTest=new LambdaParser

     // parserErrorTest.parse("lx.#")

  }
}
