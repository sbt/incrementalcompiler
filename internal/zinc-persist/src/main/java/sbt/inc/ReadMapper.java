/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package sbt.inc;

import sbt.internal.inc.mappers.RelativeReadMapper;
import xsbti.compile.MiniSetup;
import xsbti.compile.analysis.Stamp;

import java.io.File;
import java.nio.file.Path;

/**
 * Defines a reader-only mapper interface that is used by Zinc before read
 * the contents of the analysis files from the persistent storage.
 *
 * This interface is useful to make the analysis file machine-independent and
 * allow third parties to distribute these files around.
 */
public interface ReadMapper extends GenericMapper {

    /**
     * Defines a mapper that reads from a machine-independent analysis file.
     *
     * An analysis file is machine independent if all the paths are relative and no
     * information about the machine that produced it is stored -- only information
     * about the structure of the project from a given build tool is persisted.
     *
     * The mapper makes sure that the analysis file looks like if it was generated by
     * the local machine it's executed on.
     *
     * @param projectRootPath The path on which we want to "mount" all the relative paths in analysis.
     * @return A read mapper to pass in to {@link sbt.internal.inc.FileAnalysisStore}.
     */
    public static ReadMapper getMachineIndependentMapper(Path projectRootPath) {
        return new RelativeReadMapper(projectRootPath);
    }

    /**
     * Defines an no-op read mapper.
     *
     * This is useful when users are not interested in distributing the analysis files
     * and need to pass a read mapper to {@link sbt.internal.inc.FileAnalysisStore}.
     *
     * @return A no-op read mapper.
     */
    public static ReadMapper getEmptyMapper() {
        return new ReadMapper() {
            @Override
            public File mapSourceFile(File sourceFile) {
                return sourceFile;
            }

            @Override
            public File mapBinaryFile(File binaryFile) {
                return binaryFile;
            }

            @Override
            public File mapProductFile(File productFile) {
                return productFile;
            }

            @Override
            public File mapOutputDir(File outputDir) {
                return outputDir;
            }

            @Override
            public File mapSourceDir(File sourceDir) {
                return sourceDir;
            }

            @Override
            public File mapClasspathEntry(File classpathEntry) {
                return classpathEntry;
            }

            @Override
            public String mapJavacOption(String javacOption) {
                return javacOption;
            }

            @Override
            public String mapScalacOption(String scalacOption) {
                return scalacOption;
            }

            @Override
            public Stamp mapBinaryStamp(File file, Stamp binaryStamp) {
                return binaryStamp;
            }

            @Override
            public Stamp mapSourceStamp(File file, Stamp sourceStamp) {
                return sourceStamp;
            }

            @Override
            public Stamp mapProductStamp(File file, Stamp productStamp) {
                return productStamp;
            }

            @Override
            public MiniSetup mapMiniSetup(MiniSetup miniSetup) {
                return miniSetup;
            }
        };
    }
}
