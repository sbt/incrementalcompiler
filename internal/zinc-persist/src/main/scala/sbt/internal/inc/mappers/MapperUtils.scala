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

package sbt.internal.inc.mappers

import java.io.File
import java.nio.file.Path

import xsbti.compile.analysis.Stamp

object MapperUtils {
  private[inc] def rebase(target: File, from: Path, to: Path): File = {
    to.resolve(from.relativize(target.toPath)).toFile
  }

  /** Defines a marker that tells the utils that the relativized path is empty. */
  private final val RELATIVE_MARKER = "\u2603\u2603\u2603"
  private final val MARKER_LENGTH = RELATIVE_MARKER.length()

  private final def relativeWriteError(file: File, from: Path) =
    s"Path $file is not located below path $from, and so cannot be safely relativized to it."

  private final def relativeReadError(path: String) =
    s"Unexpected path $path was not written by a relative write mapper. Paths have to start with $RELATIVE_MARKER"

  /**
   * Makes a file relative to a path.
   *
   * This function converts all file paths to a relative format that has a [[RELATIVE_MARKER]]
   * prepended to the path to signal to the reader that the transformed path is indeed relative.
   *
   * This is necessary to fail-fast when writing/reading with wrong filepaths that have been
   * generated by mappers that do not use [[makeRelative()]] and [[reconstructRelative()]]
   * consistently.
   *
   * Also, the marker is important to signal when the path of `file` and `from` are the same,
   * in which case the resulting path is empty and the text format serializes it wrong (because
   * it assumes that the paths are non-empty). The marker helps us avoid this issue.
   *
   * <note>This function is is strictly tied to [[reconstructRelative()], use them together. </note>
   *
   * @return A relativized file with a special prefix to denote the path is relative.
   */
  private[inc] def makeRelative(file: File, from: Path): File = {
    val relativePath = from.relativize(file.toPath)
    if (relativePath.startsWith("..")) {
      throw new RelativePathAssumptionBroken(relativeWriteError(file, from))
    }
    new File(s"$RELATIVE_MARKER${relativePath}")
  }

  /**
   * Reconstructs a file from a given path, making the file absolute.
   *
   * This function, as its counterpart [[makeRelative()]], assumes that the relative
   * file paths have been generated by [[makeRelative()]] and therefore fails-fast if
   * some assumptions are not met (the prefix has to be [[RELATIVE_MARKER]]).
   *
   * @return An absolute path from a relativized file by [[makeRelative()]].
   */
  private[inc] def reconstructRelative(file: File, from: Path): File = {
    val filePath = file.toPath.toString
    if (filePath.startsWith(RELATIVE_MARKER)) {
      val cleanPath = filePath.drop(MARKER_LENGTH)
      from.resolve(cleanPath).toFile()
    } else throw new RelativePathAssumptionBroken(relativeReadError(filePath))
  }

  private final class RelativePathAssumptionBroken(msg: String) extends Exception(msg)

  private[inc] def recomputeModificationDate(previouslyStampedFile: File): Stamp = {
    sbt.internal.inc.Stamper.forLastModified(previouslyStampedFile)
  }
}
