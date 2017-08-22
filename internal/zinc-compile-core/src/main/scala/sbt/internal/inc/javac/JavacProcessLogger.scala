/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt

package internal
package inc
package javac

import xsbti._
import java.io.File
import scala.sys.process.ProcessLogger

/**
 * An adapted process logger which can feed semantic error events from Javac as well as just
 * dump logs.
 *
 *
 * @param log  The logger where all input will go.
 * @param reporter  A reporter for semantic Javac error messages.
 * @param cwd The current working directory of the Javac process, used when parsing Filenames.
 */
final class JavacLogger(log: sbt.util.Logger, reporter: Reporter, cwd: File) extends ProcessLogger {
  import scala.collection.mutable.ListBuffer
  import sbt.util.Level.{ Info, Error, Value => LogLevel }

  private val msgs: ListBuffer[(LogLevel, String)] = new ListBuffer()

  def out(s: => String): Unit =
    synchronized { msgs += ((Info, s)); () }

  def err(s: => String): Unit =
    synchronized { msgs += ((Error, s)); () }

  def buffer[T](f: => T): T = f

  // Helper method to dump all semantic errors.
  private def parseAndDumpSemanticErrors(): Unit = {
    val input =
      msgs collect {
        case (Error, msg) => msg
      } mkString "\n"
    val parser = new JavaErrorParser(cwd)
    parser.parseProblems(input, log).foreach(reporter.log)
  }

  def flush(exitCode: Int): Unit = {
    parseAndDumpSemanticErrors()
    // Here we only display things that wouldn't otherwise be output by the error reporter.
    // TODO - NOTES may not be displayed correctly!
    msgs collect {
      case (Info, msg) => msg
    } foreach { msg =>
      log.info(msg)
    }
    msgs.clear()
  }
}
