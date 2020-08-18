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

/**
 * CompileOrder defines the order in which Scala and Java compilation
 * are called within a single cycle of incremental compilation.
 * The motivation for CompileOrder is a minor performance improvement
 * we expect from skipping to parse Java sources.
 *
 * In general, because incremental compilatation happens over the course
 * of time as a series of direct and indirect invalidation and compilation,
 * the ordering here is more of a implementation detail rather than an
 * invariant you can rely on to enforce visibility etc.
 */
public enum CompileOrder {
  /**
   * Scalac is called then Javac is called. This is the recommended default.
   *
   * Under this mode, both Java and Scala sources can depend on each other
   * at all times.
   * Java sources are also passed to the Scala compiler, which parses them,
   * populates the symbol table and lifts them to Scala trees without
   * generating class files for the Java trees.
   * Then the incremental compiler will add the generated Scala class files
   * to the classpath of the Java compiler so that Java sources can depend
   * on Scala sources.
   */
  Mixed,

  /**
   * Javac is called then Scalac is called.
   *
   * When mixed compilation is NOT required, it's generally more efficient
   * to use {@link CompileOrder#JavaThenScala} than {@link CompileOrder#Mixed}
   * because the Scala compiler will not parse Java sources: it will just
   * unpickle the symbol information from class files.
   *
   * Because Javac is called first, during the first round of compilation
   * it does not allow Java sources to depend on Scala sources.
   * Scala sources can depend on the Java sources at all times.
   */
  JavaThenScala,

  /**
   * Scalac is called without Java sources, then Javac is called.
   *
   * When mixed compilation is NOT required, it's generally more efficient
   * to use {@link CompileOrder#ScalaThenJava} than {@link CompileOrder#Mixed}
   * because the Scala compiler will not parse Java sources.
   *
   * The downside is that because Java sources are not passed to Scalac,
   * during the first round of compilation it does not allow Scala sources
   * to depend on Java sources.
   *
   * @deprecated Use {@link CompileOrder#Mixed} instead.
   */
  @Deprecated
  ScalaThenJava
}
