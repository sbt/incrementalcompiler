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

package xsbti.compile;

import xsbti.AnalysisCallback;
import xsbti.FileConverter;
import xsbti.Logger;
import xsbti.Reporter;
import xsbti.VirtualFile;

import java.io.File;
import java.util.Optional;

/**
 * Represent the interface of a Scala compiler.
 */
public interface ScalaCompiler {
  /**
   * Return the {@link ScalaInstance} used by this instance of the compiler.
   */
  ScalaInstance scalaInstance();

  /**
   * Return the {@link ClasspathOptions} used by this instance of the compiler.
   */
  ClasspathOptions classpathOptions();

  /**
   * Return 2 for CompilerInterface2.
   * Return 1 for CompilerInterface1.
   * Return 0 otherwise.
   */
  int bridgeCompatibilityLevel(Logger log);

  /**
   * Recompile the subset of <code>sources</code> impacted by the
   * changes defined in <code>changes</code> and collect the new APIs.
   *
   * @param sources     All the sources of the project.
   * @param changes     The changes that have been detected at the previous step.
   * @param options     The arguments to give to the Scala compiler.
   *                    For more information, run `scalac -help`.
   * @param output      The location where generated class files should be put.
   * @param callback    The callback to which the extracted information should
   *                    be reported.
   * @param reporter    The reporter to which errors and warnings should be
   *                    reported during compilation.
   * @param cache       The cache from where we retrieve the compiler to use.
   * @param log         The logger in which the Scala compiler will log info.
   * @param progressOpt The progress interface in which the Scala compiler
   *                    will report on the file being compiled.
   */
  void compile(File[] sources,
               DependencyChanges changes,
               String[] options,
               Output output,
               AnalysisCallback callback,
               Reporter reporter,
               GlobalsCache cache,
               Optional<CompileProgress> progressOpt,
               Logger log);

  /**
   * Recompile the subset of <code>sources</code> impacted by the
   * changes defined in <code>changes</code> and collect the new APIs.
   *
   * @param sources     All the sources of the project.
   * @param changes     The changes that have been detected at the previous step.
   * @param options     The arguments to give to the Scala compiler.
   *                    For more information, run `scalac -help`.
   * @param output      The location where generated class files should be put.
   * @param callback    The callback to which the extracted information should
   *                    be reported.
   * @param reporter    The reporter to which errors and warnings should be
   *                    reported during compilation.
   * @param cache       The cache from where we retrieve the compiler to use.
   * @param log         The logger in which the Scala compiler will log info.
   * @param progressOpt The progress interface in which the Scala compiler
   *                    will report on the file being compiled.
   */
  void compile(VirtualFile[] sources,
               DependencyChanges changes,
               String[] options,
               Output output,
               AnalysisCallback callback,
               Reporter reporter,
               GlobalsCache cache,
               Optional<CompileProgress> progressOpt,
               Logger log);
}

