package xsbti.compile;

import java.io.File;

public interface ClassfileManager {
  /**
   * Called once per compilation step with the class files to delete prior to that step's compilation.
   * The files in `classes` must not exist if this method returns normally.
   * Any empty ancestor directories of deleted files must not exist either.
   */
  void delete(File[] classes);

  /** Called once per compilation step with the class files generated during that step.*/
  void generated(File[] classes);

  /** Called once at the end of the whole compilation run, with `success` indicating whether compilation succeeded (true) or not (false).*/
  void complete(boolean success);
}
