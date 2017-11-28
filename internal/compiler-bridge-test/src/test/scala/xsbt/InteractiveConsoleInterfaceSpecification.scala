/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbt

import sbt.internal.inc.UnitSpec
import sbt.util.Logger
import xsbti.InteractiveConsoleResult

// This is a specification to check the REPL block parsing.
class InteractiveConsoleInterfaceSpecification extends UnitSpec {

  private val consoleFactory = new InteractiveConsoleFactory

  def consoleWithArgs(args: String*) = consoleFactory.createConsole(
    args = args.toArray,
    bootClasspathString = "",
    classpathString = "",
    initialCommands = "",
    cleanupCommands = "",
    loader = this.getClass.getClassLoader,
    bindNames = Array.empty,
    bindValues = Array.empty,
    log = Logger.Null
  )

  private val consoleWithoutArgs = consoleWithArgs()

  "Scala interpreter" should "evaluate arithmetic expression" in {
    val response = consoleWithoutArgs.interpret("1+1", false)
    response.output.trim shouldBe "res0: Int = 2"
    response.result shouldBe InteractiveConsoleResult.Success
  }

  it should "evaluate list constructor" in {
    val response = consoleWithoutArgs.interpret("List(1,2)", false)
    response.output.trim shouldBe "res1: List[Int] = List(1, 2)"
    response.result shouldBe InteractiveConsoleResult.Success
  }

  it should "evaluate import" in {
    val response = consoleWithoutArgs.interpret("import xsbt._", false)
    response.output.trim shouldBe "import xsbt._"
    response.result shouldBe InteractiveConsoleResult.Success
  }

  it should "mark partial expression as incomplete" in {
    val response = consoleWithoutArgs.interpret("val a =", false)
    response.result shouldBe InteractiveConsoleResult.Incomplete
  }

  it should "not evaluate incorrect expression" in {
    val response = consoleWithoutArgs.interpret("1 ++ 1", false)
    response.result shouldBe InteractiveConsoleResult.Error
  }

  val postfixOpExpression = "import scala.concurrent.duration._\nval t = 1 second"

  it should "evaluate postfix op with a warning" in {
    val response = consoleWithoutArgs.interpret(postfixOpExpression, false)
    response.output.trim should startWith("warning")
    response.result shouldBe InteractiveConsoleResult.Success
  }

  private val consoleWithPostfixOps = consoleWithArgs("-language:postfixOps")

  it should "evaluate postfix op without warning when -language:postfixOps arg passed" in {
    val response = consoleWithPostfixOps.interpret(postfixOpExpression, false)
    response.output.trim should not startWith "warning"
    response.result shouldBe InteractiveConsoleResult.Success
  }

}
