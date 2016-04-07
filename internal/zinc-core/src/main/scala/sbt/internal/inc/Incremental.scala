/* sbt -- Simple Build Tool
 * Copyright 2010  Mark Harrah
 */
package sbt
package internal
package inc

import java.io.File
import sbt.io.IO
import sbt.util.Logger
import scala.annotation.tailrec
import xsbti.compile.{ DependencyChanges, IncOptions, CompileAnalysis, Output }

/**
 * Helper class to run incremental compilation algorithm.
 *
 *
 * This class delegates down to
 * - IncrementalNameHashing
 * - IncrementalDefault
 * - IncrementalAnyStyle
 */
object Incremental {
  class PrefixingLogger(val prefix: String)(orig: Logger) extends Logger {
    def trace(t: => Throwable): Unit = orig.trace(t)
    def success(message: => String): Unit = orig.success(message)
    def log(level: sbt.util.Level.Value, message: => String): Unit = orig.log(level, message.replaceAll("(?m)^", prefix))
  }

  /**
   * Runs the incremental compiler algorithm.
   *
   * @param sources   The sources to compile
   * @param lookup
   *              An instance of the `Lookup` that implements looking up both classpath elements
   *              and Analysis object instances by a binary class name.
   * @param current  A mechanism for generating stamps (timestamps, hashes, etc).
   * @param doCompile  The function which can run one level of compile.
   * @param log  The log where we write debugging information
   * @param options  Incremental compilation options
   * @param output  The final output location for the compile.
   * @param equivS  The means of testing whether two "Stamps" are the same.
   * @return
   *         A flag of whether or not compilation completed succesfully, and the resulting dependency analysis object.
   */
  def compile(
    sources: Set[File],
    lookup: Lookup,
    previous0: CompileAnalysis,
    current: ReadStamps,
    doCompile: (Set[File], DependencyChanges) => Analysis,
    output: Output,
    log: sbt.util.Logger,
    options: IncOptions
  )(implicit equivS: Equiv[Stamp]): (Boolean, Analysis) =
    {
      val previous = previous0 match { case a: Analysis => a }
      val incremental: IncrementalCommon =
        if (options.nameHashing)
          new IncrementalNameHashing(new PrefixingLogger("[naha] ")(log), options)
        else if (options.antStyle)
          new IncrementalAntStyle(log, options)
        else
          throw new UnsupportedOperationException("Turning off name hashing is not supported in class-based dependency tracking")
      val initialChanges = incremental.changedInitial(sources, previous, current, lookup)
      val binaryChanges = new DependencyChanges {
        val modifiedBinaries = initialChanges.binaryDeps.toArray
        val modifiedClasses = initialChanges.external.allModified.toArray
        def isEmpty = modifiedBinaries.isEmpty && modifiedClasses.isEmpty
      }
      val (initialInvClasses, initialInvSources) = incremental.invalidateInitial(previous.relations, initialChanges)
      log.debug("All initially invalidated classes: " + initialInvClasses + "\n" +
        "All initially invalidated sources:" + initialInvSources + "\n")
      val analysis = ClassfileManager.manageClassfiles(output, options) {
        incremental.cycle(initialInvClasses, initialInvSources, sources, binaryChanges, previous, doCompile, 1)
      }
      (initialInvClasses.nonEmpty || initialInvSources.nonEmpty, analysis)
    }

  // the name of system property that was meant to enable debugging mode of incremental compiler but
  // it ended up being used just to enable debugging of relations. That's why if you migrate to new
  // API for configuring incremental compiler (IncOptions) it's enough to control value of `relationsDebug`
  // flag to achieve the same effect as using `incDebugProp`.
  @deprecated("Use `IncOptions.relationsDebug` flag to enable debugging of relations.", "0.13.2")
  val incDebugProp = "xsbt.inc.debug"

  private[inc] val apiDebugProp = "xsbt.api.debug"
  private[inc] def apiDebug(options: IncOptions): Boolean = options.apiDebug || java.lang.Boolean.getBoolean(apiDebugProp)

  private[sbt] def prune(invalidatedSrcs: Set[File], previous0: CompileAnalysis): Analysis = {
    val previous = previous0 match { case a: Analysis => a }
    IO.deleteFilesEmptyDirs(invalidatedSrcs.flatMap(previous.relations.products))
    previous -- invalidatedSrcs
  }
}
