package xsbti.compile;

import java.io.File;

/**
 * Defines lookup of data structures and operations zinc needs to perform on per classpath element basis.
 */
public interface PerClasspathEntryLookup {

    /** Provides the Analysis for the given classpath entry. */
    xsbti.Maybe<CompileAnalysis> analysis(File classpathEntry);

    /**
     * Provides a function to determine if classpath entry `file` contains a given class.
     * The returned function should generally cache information about `file`, such as the list of entries in a jar.
     */
    DefinesClass definesClass(File classpathEntry);
}
