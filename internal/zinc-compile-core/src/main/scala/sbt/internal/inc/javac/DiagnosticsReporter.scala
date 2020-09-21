/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package sbt

package internal
package inc
package javac

import java.util.Optional
import java.io.File
import javax.tools.{ Diagnostic, JavaFileObject, DiagnosticListener }
import sbt.io.IO
import sbt.util.InterfaceUtil.o2jo
import xsbti.{ Severity, Reporter }
import javax.tools.Diagnostic.NOPOS

/**
 * A diagnostics listener that feeds all messages into the given reporter.
 * @param reporter
 */
final class DiagnosticsReporter(reporter: Reporter) extends DiagnosticListener[JavaFileObject] {
  import DiagnosticsReporter._

  val END_OF_LINE_MATCHER = "(\r\n)|[\r]|[\n]"
  val EOL = System.getProperty("line.separator")

  private[this] var errorEncountered = false
  def hasErrors: Boolean = errorEncountered

  override def report(d: Diagnostic[_ <: JavaFileObject]): Unit = {
    val severity: Severity = {
      d.getKind match {
        case Diagnostic.Kind.ERROR                                       => Severity.Error
        case Diagnostic.Kind.WARNING | Diagnostic.Kind.MANDATORY_WARNING => Severity.Warn
        case _                                                           => Severity.Info
      }
    }

    import sbt.util.InterfaceUtil.problem
    val msg = d.getMessage(null)
    val pos: xsbti.Position = PositionImpl(d)
    if (severity == Severity.Error) errorEncountered = true
    reporter.log(problem("", pos, msg, severity, None))
  }

}

object DiagnosticsReporter {

  /** Strict and immutable implementation of Position. */
  private[sbt] final class PositionImpl private (
      sourceUri: Option[String],
      override val line: Optional[Integer],
      override val lineContent: String,
      override val offset: Optional[Integer],
      override val startOffset: Optional[Integer],
      override val endOffset: Optional[Integer]
  ) extends xsbti.Position {
    override val sourcePath: Optional[String] = o2jo(sourceUri)
    override val sourceFile: Optional[File] = o2jo(sourceUri.map(new File(_)))
    override val pointer: Optional[Integer] = o2jo(Option.empty[Integer])
    override val pointerSpace: Optional[String] = o2jo(Option.empty[String])

    override val startLine: Optional[Integer] = o2jo(Option.empty)
    override val startColumn: Optional[Integer] = o2jo(Option.empty)
    override val endLine: Optional[Integer] = o2jo(Option.empty)
    override val endColumn: Optional[Integer] = o2jo(Option.empty)

    override def toString: String =
      if (sourceUri.isDefined) s"${sourceUri.get}:${if (line.isPresent) line.get else -1}"
      else ""

  }

  private[sbt] object PositionImpl {

    /**
     * Extracts PositionImpl from a Java Diagnostic.
     * The previous implementation of PositionImpl held on to the Diagnostic object as a wrapper,
     * and calculated the lineContent on the fly.
     * This caused a race condition on the Diagnostic object, resulting in a NullPointerException.
     * See https://github.com/sbt/sbt/issues/3623
     */
    def apply(d: Diagnostic[_ <: JavaFileObject]): PositionImpl = {
      // https://docs.oracle.com/javase/7/docs/api/javax/tools/Diagnostic.html
      // Negative values (except NOPOS) and 0 are not valid line or column numbers,
      // except that you can cause this number to occur by putting "abc {}" in A.java.
      // This will cause Invalid position: 0 masking the actual error message
      //     a/A.java:1: class, interface, or enum expected
      def checkNoPos(n: Long): Option[Long] = n match {
        case NOPOS       => None
        case x if x <= 0 => None
        case x           => Option(x)
      }

      val source: Option[JavaFileObject] = Option(d.getSource)

      // see also LocalJava.scala
      val sourcePath: Option[String] =
        source match {
          case Some(obj) =>
            val uri = obj.toUri
            if (uri.getScheme == "file")
              Some(
                IO.urlAsFile(uri.toURL)
                  .map(_.getAbsolutePath)
                  .getOrElse(uri.toString)
              )
            else if (uri.getScheme == "vf")
              Some(LocalJava.fromUri(uri).id)
            else Some(uri.toString)
          case _ => None
        }
      val line: Optional[Integer] = o2jo(checkNoPos(d.getLineNumber).map(_.toInt))
      val offset: Optional[Integer] = o2jo(checkNoPos(d.getPosition).map(_.toInt))
      val startOffset: Optional[Integer] = o2jo(checkNoPos(d.getStartPosition).map(_.toInt))
      val endOffset: Optional[Integer] = o2jo(checkNoPos(d.getEndPosition).map(_.toInt))

      def startPosition: Option[Long] = checkNoPos(d.getStartPosition)
      def endPosition: Option[Long] = checkNoPos(d.getEndPosition)
      // TODO - Is this pulling contents of the line correctly?
      // Would be ok to just return null if this version of the JDK doesn't support grabbing
      // source lines?
      val lineContent: String =
        source match {
          case Some(source: JavaFileObject) =>
            (Option(source.getCharContent(true)), startPosition, endPosition) match {
              case (Some(cc), Some(start), Some(end)) =>
                cc.subSequence(start.toInt, end.toInt).toString
              case _ => ""
            }
          case _ => ""
        }

      new PositionImpl(sourcePath, line, lineContent, offset, startOffset, endOffset)
    }

  }

}
