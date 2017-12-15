/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt
package internal
package inc
package classpath

import java.lang.ref.{ Reference, SoftReference }
import java.io.File
import java.net.URLClassLoader
import java.util.HashMap
import sbt.io.IO

// Hack for testing only
final class ClassLoaderCache(val commonParent: ClassLoader) {
  private[this] val delegate =
    new HashMap[List[File], Reference[CachedClassLoader]]

  /**
   * Returns a ClassLoader with `commonParent` as a parent and that will load classes from classpath `files`.
   * The returned ClassLoader may be cached from a previous call if the last modified time of all `files` is unchanged.
   * This method is thread-safe.
   */
  def apply(files: List[File]): ClassLoader = synchronized {
    cachedCustomClassloader(
      files,
      () => new URLClassLoader(files.map(_.toURI.toURL).toArray, commonParent)
    )
  }

  /**
   * Returns a ClassLoader, as created by `mkLoader`.
   *
   * The returned ClassLoader may be cached from a previous call if the last modified time of all `files` is unchanged.
   * This method is thread-safe.
   */
  def cachedCustomClassloader(
      files: List[File],
      mkLoader: () => ClassLoader
  ): ClassLoader =
    synchronized {
      val tstamps = files.map(IO.getModifiedTime)
      getFromReference(files, tstamps, delegate.get(files), mkLoader)
    }

  private[this] def getFromReference(
      files: List[File],
      stamps: List[Long],
      existingRef: Reference[CachedClassLoader],
      mkLoader: () => ClassLoader
  ) =
    if (existingRef eq null)
      newEntry(files, stamps, mkLoader)
    else
      get(files, stamps, existingRef.get, mkLoader)

  private[this] def get(
      files: List[File],
      stamps: List[Long],
      existing: CachedClassLoader,
      mkLoader: () => ClassLoader
  ): ClassLoader =
    if (existing == null || stamps != existing.timestamps) {
      newEntry(files, stamps, mkLoader)
    } else
      existing.loader

  private[this] def newEntry(
      files: List[File],
      stamps: List[Long],
      mkLoader: () => ClassLoader
  ): ClassLoader = {
    val loader = mkLoader()
    delegate.put(
      files,
      new SoftReference(new CachedClassLoader(loader, files, stamps))
    )
    loader
  }
}
private[sbt] final class CachedClassLoader(
    val loader: ClassLoader,
    val files: List[File],
    val timestamps: List[Long]
)
