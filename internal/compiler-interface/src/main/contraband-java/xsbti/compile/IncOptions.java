/**
 * This code is generated using [[https://www.scala-sbt.org/contraband/ sbt-contraband]].
 */

// DO NOT EDIT MANUALLY
package xsbti.compile;
/**
 * Represents all configuration options for the incremental compiler itself and
 * not the underlying Java/Scala compiler.
 */
public final class IncOptions implements java.io.Serializable {
    public static int defaultTransitiveStep() {
        return 3;
    }
    public static double defaultRecompileAllFraction() {
        return 0.5;
    }
    public static boolean defaultRelationsDebug() {
        return false;
    }
    public static boolean defaultApiDebug() {
        return false;
    }
    public static int defaultApiDiffContextSize() {
        return 5;
    }
    public static java.util.Optional<java.io.File> defaultApiDumpDirectory() {
        return java.util.Optional.empty();
    }
    public static java.util.Optional<ClassFileManagerType> defaultClassFileManagerType() {
        return java.util.Optional.empty();
    }
    public static java.util.Optional<Boolean> defaultRecompileOnMacroDef() {
        return java.util.Optional.empty();
    }
    public static boolean defaultUseOptimizedSealed() {
        return false;
    }
    public static boolean defaultRecompileOnMacroDefImpl() {
        return true;
    }
    public static boolean getRecompileOnMacroDef(IncOptions options) {
        if (options.recompileOnMacroDef().isPresent()) {
            return options.recompileOnMacroDef().get();
        } else {
            return defaultRecompileOnMacroDefImpl();
        }
    }
    public static boolean defaultUseCustomizedFileManager() {
        return false;
    }
    public static boolean defaultStoreApis() {
        return true;
    }
    public static boolean defaultEnabled() {
        return true;
    }
    public static java.util.Map<String, String> defaultExtra() {
        return new java.util.HashMap<String, String>();
    }
    public static ExternalHooks defaultExternal() {
        return new DefaultExternalHooks(java.util.Optional.empty(), java.util.Optional.empty());
    }
    public static String[] defaultIgnoredScalacOptions() {
        return new String[0];
    }
    public static boolean defaultLogRecompileOnMacro() {
        return true;
    }
    public static boolean defaultStrictMode() {
        return false;
    }
    public static boolean defaultAllowMachinePath() {
        return true;
    }
    public static boolean defaultPipelining() {
        return false;
    }
    public static IncOptions create() {
        return new IncOptions();
    }
    public static IncOptions of() {
        return new IncOptions();
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode, boolean _allowMachinePath, boolean _pipelining) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode, _allowMachinePath, _pipelining);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode, boolean _allowMachinePath, boolean _pipelining) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode, _allowMachinePath, _pipelining);
    }
    public static IncOptions create(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode, boolean _allowMachinePath, boolean _pipelining) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode, _allowMachinePath, _pipelining);
    }
    public static IncOptions of(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode, boolean _allowMachinePath, boolean _pipelining) {
        return new IncOptions(_transitiveStep, _recompileAllFraction, _relationsDebug, _apiDebug, _apiDiffContextSize, _apiDumpDirectory, _classfileManagerType, _useCustomizedFileManager, _recompileOnMacroDef, _useOptimizedSealed, _storeApis, _enabled, _extra, _logRecompileOnMacro, _externalHooks, _ignoredScalacOptions, _strictMode, _allowMachinePath, _pipelining);
    }
    private int transitiveStep;
    private double recompileAllFraction;
    private boolean relationsDebug;
    private boolean apiDebug;
    private int apiDiffContextSize;
    private java.util.Optional<java.io.File> apiDumpDirectory;
    private java.util.Optional<xsbti.compile.ClassFileManagerType> classfileManagerType;
    private boolean useCustomizedFileManager;
    private java.util.Optional<Boolean> recompileOnMacroDef;
    private boolean useOptimizedSealed;
    private boolean storeApis;
    private boolean enabled;
    private java.util.Map<String, String> extra;
    private boolean logRecompileOnMacro;
    private xsbti.compile.ExternalHooks externalHooks;
    private String[] ignoredScalacOptions;
    private boolean strictMode;
    private boolean allowMachinePath;
    private boolean pipelining;
    protected IncOptions() {
        super();
        transitiveStep = xsbti.compile.IncOptions.defaultTransitiveStep();
        recompileAllFraction = xsbti.compile.IncOptions.defaultRecompileAllFraction();
        relationsDebug = xsbti.compile.IncOptions.defaultRelationsDebug();
        apiDebug = xsbti.compile.IncOptions.defaultApiDebug();
        apiDiffContextSize = xsbti.compile.IncOptions.defaultApiDiffContextSize();
        apiDumpDirectory = xsbti.compile.IncOptions.defaultApiDumpDirectory();
        classfileManagerType = xsbti.compile.IncOptions.defaultClassFileManagerType();
        useCustomizedFileManager = xsbti.compile.IncOptions.defaultUseCustomizedFileManager();
        recompileOnMacroDef = xsbti.compile.IncOptions.defaultRecompileOnMacroDef();
        useOptimizedSealed = xsbti.compile.IncOptions.defaultUseOptimizedSealed();
        storeApis = xsbti.compile.IncOptions.defaultStoreApis();
        enabled = xsbti.compile.IncOptions.defaultEnabled();
        extra = xsbti.compile.IncOptions.defaultExtra();
        logRecompileOnMacro = xsbti.compile.IncOptions.defaultLogRecompileOnMacro();
        externalHooks = xsbti.compile.IncOptions.defaultExternal();
        ignoredScalacOptions = xsbti.compile.IncOptions.defaultIgnoredScalacOptions();
        strictMode = xsbti.compile.IncOptions.defaultStrictMode();
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = _apiDumpDirectory;
        classfileManagerType = _classfileManagerType;
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = _recompileOnMacroDef;
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = xsbti.compile.IncOptions.defaultIgnoredScalacOptions();
        strictMode = xsbti.compile.IncOptions.defaultStrictMode();
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = java.util.Optional.<java.io.File>ofNullable(_apiDumpDirectory);
        classfileManagerType = java.util.Optional.<xsbti.compile.ClassFileManagerType>ofNullable(_classfileManagerType);
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = java.util.Optional.<Boolean>ofNullable(_recompileOnMacroDef);
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = xsbti.compile.IncOptions.defaultIgnoredScalacOptions();
        strictMode = xsbti.compile.IncOptions.defaultStrictMode();
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = _apiDumpDirectory;
        classfileManagerType = _classfileManagerType;
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = _recompileOnMacroDef;
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = _ignoredScalacOptions;
        strictMode = xsbti.compile.IncOptions.defaultStrictMode();
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = java.util.Optional.<java.io.File>ofNullable(_apiDumpDirectory);
        classfileManagerType = java.util.Optional.<xsbti.compile.ClassFileManagerType>ofNullable(_classfileManagerType);
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = java.util.Optional.<Boolean>ofNullable(_recompileOnMacroDef);
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = _ignoredScalacOptions;
        strictMode = xsbti.compile.IncOptions.defaultStrictMode();
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = _apiDumpDirectory;
        classfileManagerType = _classfileManagerType;
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = _recompileOnMacroDef;
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = _ignoredScalacOptions;
        strictMode = _strictMode;
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = java.util.Optional.<java.io.File>ofNullable(_apiDumpDirectory);
        classfileManagerType = java.util.Optional.<xsbti.compile.ClassFileManagerType>ofNullable(_classfileManagerType);
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = java.util.Optional.<Boolean>ofNullable(_recompileOnMacroDef);
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = _ignoredScalacOptions;
        strictMode = _strictMode;
        allowMachinePath = xsbti.compile.IncOptions.defaultAllowMachinePath();
        pipelining = xsbti.compile.IncOptions.defaultPipelining();
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.util.Optional<java.io.File> _apiDumpDirectory, java.util.Optional<xsbti.compile.ClassFileManagerType> _classfileManagerType, boolean _useCustomizedFileManager, java.util.Optional<Boolean> _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode, boolean _allowMachinePath, boolean _pipelining) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = _apiDumpDirectory;
        classfileManagerType = _classfileManagerType;
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = _recompileOnMacroDef;
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = _ignoredScalacOptions;
        strictMode = _strictMode;
        allowMachinePath = _allowMachinePath;
        pipelining = _pipelining;
    }
    protected IncOptions(int _transitiveStep, double _recompileAllFraction, boolean _relationsDebug, boolean _apiDebug, int _apiDiffContextSize, java.io.File _apiDumpDirectory, xsbti.compile.ClassFileManagerType _classfileManagerType, boolean _useCustomizedFileManager, boolean _recompileOnMacroDef, boolean _useOptimizedSealed, boolean _storeApis, boolean _enabled, java.util.Map<String, String> _extra, boolean _logRecompileOnMacro, xsbti.compile.ExternalHooks _externalHooks, String[] _ignoredScalacOptions, boolean _strictMode, boolean _allowMachinePath, boolean _pipelining) {
        super();
        transitiveStep = _transitiveStep;
        recompileAllFraction = _recompileAllFraction;
        relationsDebug = _relationsDebug;
        apiDebug = _apiDebug;
        apiDiffContextSize = _apiDiffContextSize;
        apiDumpDirectory = java.util.Optional.<java.io.File>ofNullable(_apiDumpDirectory);
        classfileManagerType = java.util.Optional.<xsbti.compile.ClassFileManagerType>ofNullable(_classfileManagerType);
        useCustomizedFileManager = _useCustomizedFileManager;
        recompileOnMacroDef = java.util.Optional.<Boolean>ofNullable(_recompileOnMacroDef);
        useOptimizedSealed = _useOptimizedSealed;
        storeApis = _storeApis;
        enabled = _enabled;
        extra = _extra;
        logRecompileOnMacro = _logRecompileOnMacro;
        externalHooks = _externalHooks;
        ignoredScalacOptions = _ignoredScalacOptions;
        strictMode = _strictMode;
        allowMachinePath = _allowMachinePath;
        pipelining = _pipelining;
    }
    /** After which step include whole transitive closure of invalidated source files. */
    public int transitiveStep() {
        return this.transitiveStep;
    }
    /**
     * What's the fraction of invalidated source files when we switch to recompiling
     * all files and giving up incremental compilation altogether. That's useful in
     * cases when probability that we end up recompiling most of source files but
     * in multiple steps is high. Multi-step incremental recompilation is slower
     * than recompiling everything in one step.
     */
    public double recompileAllFraction() {
        return this.recompileAllFraction;
    }
    /** Print very detailed information about relations, such as dependencies between source files. */
    public boolean relationsDebug() {
        return this.relationsDebug;
    }
    /** Enable tools for debugging API changes. */
    public boolean apiDebug() {
        return this.apiDebug;
    }
    /**
     * Controls context size (in lines) displayed when diffs are produced for textual API
     * representation.
     * 
     * This option is used only when <code>apiDebug == true</code>.
     */
    public int apiDiffContextSize() {
        return this.apiDiffContextSize;
    }
    /**
     * The directory where we dump textual representation of APIs. This method might be called
     * only if apiDebug returns true. This is unused option at the moment as the needed functionality
     * is not implemented yet.
     */
    public java.util.Optional<java.io.File> apiDumpDirectory() {
        return this.apiDumpDirectory;
    }
    /** ClassfileManager that will handle class file deletion and addition during a single incremental compilation run. */
    public java.util.Optional<xsbti.compile.ClassFileManagerType> classfileManagerType() {
        return this.classfileManagerType;
    }
    /**
     * Option to turn on customized file manager that tracks generated class files for transactional rollbacks.
     * Using customized file manager may conflict with some libraries, this option allows user to decide
     * whether to use.
     */
    public boolean useCustomizedFileManager() {
        return this.useCustomizedFileManager;
    }
    /**
     * Determines whether incremental compiler should recompile all dependencies of a file
     * that contains a macro definition.
     */
    public java.util.Optional<Boolean> recompileOnMacroDef() {
        return this.recompileOnMacroDef;
    }
    /**
     * Determines whether optimized approach for invalidating sealed classes/trait is used.
     * Turning this on may cause undercompilation in case of macros that are based sealed
     * trait/class children enumeration.
     */
    public boolean useOptimizedSealed() {
        return this.useOptimizedSealed;
    }
    /** Determines whether incremental compiler stores apis alongside analysis. */
    public boolean storeApis() {
        return this.storeApis;
    }
    /** Determines whether incremental compilation is enabled. */
    public boolean enabled() {
        return this.enabled;
    }
    /** Extra options */
    public java.util.Map<String, String> extra() {
        return this.extra;
    }
    /** Determines whether to log information on file recompiled due to a transitive macro change */
    public boolean logRecompileOnMacro() {
        return this.logRecompileOnMacro;
    }
    /** External hooks that allows clients e.g. IDEs to interacts with compilation internals */
    public xsbti.compile.ExternalHooks externalHooks() {
        return this.externalHooks;
    }
    /** Array of regexes that will be used to determine if scalac options should be ignored if they change */
    public String[] ignoredScalacOptions() {
        return this.ignoredScalacOptions;
    }
    /**
     * Enable assertions and other runtime checks that are otherwise disabled.
     * Can be useful for debugging incremental compilation issues.
     */
    public boolean strictMode() {
        return this.strictMode;
    }
    /**
     * When set to true, this makes the mapped file converter strict,
     * so no paths will include machine-specific absolute path.
     */
    public boolean allowMachinePath() {
        return this.allowMachinePath;
    }
    /** Enables build pipelining at the module level. */
    public boolean pipelining() {
        return this.pipelining;
    }
    public IncOptions withTransitiveStep(int transitiveStep) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withRecompileAllFraction(double recompileAllFraction) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withRelationsDebug(boolean relationsDebug) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withApiDebug(boolean apiDebug) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withApiDiffContextSize(int apiDiffContextSize) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withApiDumpDirectory(java.util.Optional<java.io.File> apiDumpDirectory) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withApiDumpDirectory(java.io.File apiDumpDirectory) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, java.util.Optional.<java.io.File>ofNullable(apiDumpDirectory), classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withClassfileManagerType(java.util.Optional<xsbti.compile.ClassFileManagerType> classfileManagerType) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withClassfileManagerType(xsbti.compile.ClassFileManagerType classfileManagerType) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, java.util.Optional.<xsbti.compile.ClassFileManagerType>ofNullable(classfileManagerType), useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withUseCustomizedFileManager(boolean useCustomizedFileManager) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withRecompileOnMacroDef(java.util.Optional<Boolean> recompileOnMacroDef) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withRecompileOnMacroDef(boolean recompileOnMacroDef) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, java.util.Optional.<Boolean>ofNullable(recompileOnMacroDef), useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withUseOptimizedSealed(boolean useOptimizedSealed) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withStoreApis(boolean storeApis) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withEnabled(boolean enabled) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withExtra(java.util.Map<String, String> extra) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withLogRecompileOnMacro(boolean logRecompileOnMacro) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withExternalHooks(xsbti.compile.ExternalHooks externalHooks) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withIgnoredScalacOptions(String[] ignoredScalacOptions) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withStrictMode(boolean strictMode) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withAllowMachinePath(boolean allowMachinePath) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public IncOptions withPipelining(boolean pipelining) {
        return new IncOptions(transitiveStep, recompileAllFraction, relationsDebug, apiDebug, apiDiffContextSize, apiDumpDirectory, classfileManagerType, useCustomizedFileManager, recompileOnMacroDef, useOptimizedSealed, storeApis, enabled, extra, logRecompileOnMacro, externalHooks, ignoredScalacOptions, strictMode, allowMachinePath, pipelining);
    }
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (!(obj instanceof IncOptions)) {
            return false;
        } else {
            IncOptions o = (IncOptions)obj;
            return (this.transitiveStep() == o.transitiveStep()) && (this.recompileAllFraction() == o.recompileAllFraction()) && (this.relationsDebug() == o.relationsDebug()) && (this.apiDebug() == o.apiDebug()) && (this.apiDiffContextSize() == o.apiDiffContextSize()) && this.apiDumpDirectory().equals(o.apiDumpDirectory()) && this.classfileManagerType().equals(o.classfileManagerType()) && (this.useCustomizedFileManager() == o.useCustomizedFileManager()) && this.recompileOnMacroDef().equals(o.recompileOnMacroDef()) && (this.useOptimizedSealed() == o.useOptimizedSealed()) && (this.storeApis() == o.storeApis()) && (this.enabled() == o.enabled()) && this.extra().equals(o.extra()) && (this.logRecompileOnMacro() == o.logRecompileOnMacro()) && this.externalHooks().equals(o.externalHooks()) && java.util.Arrays.deepEquals(this.ignoredScalacOptions(), o.ignoredScalacOptions()) && (this.strictMode() == o.strictMode()) && (this.allowMachinePath() == o.allowMachinePath()) && (this.pipelining() == o.pipelining());
        }
    }
    public int hashCode() {
        return 37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (37 * (17 + "xsbti.compile.IncOptions".hashCode()) + Integer.valueOf(transitiveStep()).hashCode()) + Double.valueOf(recompileAllFraction()).hashCode()) + Boolean.valueOf(relationsDebug()).hashCode()) + Boolean.valueOf(apiDebug()).hashCode()) + Integer.valueOf(apiDiffContextSize()).hashCode()) + apiDumpDirectory().hashCode()) + classfileManagerType().hashCode()) + Boolean.valueOf(useCustomizedFileManager()).hashCode()) + recompileOnMacroDef().hashCode()) + Boolean.valueOf(useOptimizedSealed()).hashCode()) + Boolean.valueOf(storeApis()).hashCode()) + Boolean.valueOf(enabled()).hashCode()) + extra().hashCode()) + Boolean.valueOf(logRecompileOnMacro()).hashCode()) + externalHooks().hashCode()) + java.util.Arrays.deepHashCode(ignoredScalacOptions())) + Boolean.valueOf(strictMode()).hashCode()) + Boolean.valueOf(allowMachinePath()).hashCode()) + Boolean.valueOf(pipelining()).hashCode());
    }
    public String toString() {
        return "IncOptions("  + "transitiveStep: " + transitiveStep() + ", " + "recompileAllFraction: " + recompileAllFraction() + ", " + "relationsDebug: " + relationsDebug() + ", " + "apiDebug: " + apiDebug() + ", " + "apiDiffContextSize: " + apiDiffContextSize() + ", " + "apiDumpDirectory: " + apiDumpDirectory() + ", " + "classfileManagerType: " + classfileManagerType() + ", " + "useCustomizedFileManager: " + useCustomizedFileManager() + ", " + "recompileOnMacroDef: " + recompileOnMacroDef() + ", " + "useOptimizedSealed: " + useOptimizedSealed() + ", " + "storeApis: " + storeApis() + ", " + "enabled: " + enabled() + ", " + "extra: " + extra() + ", " + "logRecompileOnMacro: " + logRecompileOnMacro() + ", " + "externalHooks: " + externalHooks() + ", " + "ignoredScalacOptions: " + ignoredScalacOptions() + ", " + "strictMode: " + strictMode() + ", " + "allowMachinePath: " + allowMachinePath() + ", " + "pipelining: " + pipelining() + ")";
    }
}
