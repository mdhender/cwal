/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   s2sh: the s2 shell

   Intended uses of this code:

   1) A copy/paste bootstrap for creating new s2-using clients. To
   that end, this file "should" be commented relatively well.

   2) An extensible shell for use in running client-defined
   functionality. Extensibility comes in these forms:

   2.1) Via editing this and/or shell_common.c file (discourged, as i
   copy it amongst several trees ;). Note that shell_common.c is used
   by both this file and shell2.c (its successor).

   2.2) Editing shell_extend.c and defining S2_SHELL_EXTEND when
   building this file. Preferred, as this allows clients to statically
   link in their features to the shell.

   2.3) A platform for loading new s2 features via DLLs.

   2.4) provide an auto-load feature which clients can hook into to
   extend the script-side environment during shell startup.

   3) Running s2 script files in batch mode or via an interactive
   session (requires 3rd-party libs described later in these sources).

   The non-API s2 docs include much more detail on the above points:

   https://fossil.wanderinghorse.net/r/cwal/doc/trunk/s2/

   (Or, if you're reading this from the canonical s2 source tree, see
   index.md in this file's directory.)


   Concepts needed to make any sense of this file:

   - cwal is a separate lib (bundled with s2) which provides
   the core-most features: Value system, memory management/lifetime,
   and garbage collector.

   - s2 is a script language built on that engine.

   Thus clients will see many mixes of the two in this code. In
   practice, most client code uses only the cwal-level APIs, but this
   app's level of code uses the s2 API relatively often as well.

*/

#define S2SH_VERSION 1
#include "shell_common.c"

/**
   gcc bug: if this function is marked static here, it does
   not recognize that shell_common.c uses it and declares that
   it's unused.
*/
CliAppSwitch const * s2sh_cliapp_switches(){
  static const CliAppSwitch cliAppSwitches[] = {
#define F6(IDFLAGS,DASHES,KEY,VAL,BRIEF,PFLAGS)     \
  {IDFLAGS, DASHES, KEY, VAL, BRIEF, 0, 0, PFLAGS}
#define F5(IDFLAGS,DASHES,KEY,VAL,BRIEF)        \
  F6(IDFLAGS,DASHES,KEY,VAL,BRIEF,0)
  /** Continues the help text for the previous entry. */
#define FCONTINUE(LABEL) {SW_CONTINUE, 0, 0, 0, LABEL, 0, 0, 0}
  /** Adds an "additional info" line, not indented. */
#define FINFOLINE(LABEL) {SW_INFOLINE, 0, 0, 0, LABEL, 0, 0, 0}
  /** -short | --long[=VALUE]*/
#define FX(IDFLAGS,SHORT,LONG,VALUE,BRIEF,PFLAGS)   \
  F6(IDFLAGS,2,LONG,VALUE,BRIEF,PFLAGS),            \
  F6(IDFLAGS,1,SHORT,VALUE,0,PFLAGS)
  /** -short, --long on/off switches ...*/
#define F01(IDFLAGS,KEY,BRIEF)                          \
  {IDFLAGS,   2, KEY, 0, "Disable "  BRIEF, 0, 0, 0},   \
  {IDFLAGS+1, 1, KEY, 0, "Enable "  BRIEF, 0, 0, 0}
  /** Start the next group (for --help grouping). */
#define GROUP(GID,DESC) {GID, 0, 0, 0, DESC, 0, 0, 0}

  /***********************************************************************/
  GROUP(SW_GROUP_0,0),

  F5(SW_HELP, 2, "help", 0,
     "Send help text to stdout and exit with code 0."),
  F5(SW_HELP, 1, "?", 0, 0),
  F5(SW_HELP | SW_UNDOCUMENTED, 1, "help", 0, 0),
  FCONTINUE("The -v flag can be used to get more info."),
  
  F5(SW_VERBOSE, 1, "v", 0, "Increase verbosity level by one."),
  FCONTINUE("Use with -?/--help to increase help info level."),

  F5(SW_VERSION,2,"version",0,
     "Show version info and exit with code 0. "),
  FCONTINUE("Use with -v for more details."),

  F6(SW_OUTFILE,1,"o","outfile",
     "Sets the engine's default output channel. Default=stdout.",
     CLIAPP_F_SPACE_VALUE | CLIAPP_F_ONCE),

  F6(SW_INFILE,1, "f", "infile",
     "Runs the given script file in a new scope.",
     CLIAPP_F_SPACE_VALUE | CLIAPP_F_ONCE),
  FCONTINUE("Defaults to the first non-flag argument."),

  FX(SW_ESCRIPT,"e","eval","SCRIPT",
     "Evaluates the given script code in the global scope. "
     "May be used multiple times.", CLIAPP_F_SPACE_VALUE),

  /***********************************************************************/
  GROUP(SW_GROUP_1,"Less-common options:"),

  F01(SW_ASSERT_0,"A","assert tracing."),
  FCONTINUE("Enable twice to also trace affirm calls."),

  F5(SW_INTERACTIVE,1,"i",0,
     "Enter interactive mode even if -e/-f disable it."),

  F01(SW_AUTOLOAD_0,"a","autoloading of init script."),
  FCONTINUE("Init script name = same as this binary plus \".s2\" "),
  FCONTINUE("or set via the S2SH_INIT_SCRIPT environment variable."),

  /***********************************************************************/
  GROUP(SW_GROUP_2,"Esoteric options:"),

  F5(SW_METRICS, 1, "m", 0, "Dump cwal/s2 metrics before shutdown."),
  FCONTINUE("Use -v 1-3 times for more information."),

  F01(SW_MOD_INIT_0,"M","initialization of built-in static modules."),
  F01(SW_SHELL_API_0,"s2.shell","installation of s2.shell API."),
  FCONTINUE("By default s2.shell is only installed in interactive mode."),

  F6(SW_HIST_FILE_1,1,"h","filename", "Set editing history file.",
     CLIAPP_F_SPACE_VALUE | CLIAPP_F_ONCE),
  F5(SW_HIST_FILE_0,2,"h",0, "Disable editing history file."),

  F5(SW_CLEANROOM, 1, "cleanroom", 0,
     "Do not install any global/prototype-level functionality."),
  FCONTINUE("Disables the s2 global object and auto-loading of the "
            "init script."),

  F5(SW_SHOW_SIZEOFS,1,"z",0,"Dump various sizeof() values."),

  F5(SW_INTERNAL_FU,1,"fu",0,
     "May or may not enable some internal testing. Only for use in developing this app."),

  F5(SW_CLAMPDOWN,-1,"clampdown",0,"No longer available: use --s2-disable instead."),

  F6(SW_DISABLE, 2, "s2-disable", "comma,list",
     "Disables certain s2 API features by name.",0),
  FCONTINUE("Use a comma-and/or-space-separated list with any of "
            "the following entries:"),
  FCONTINUE("fs-stat, fs-read, fs-write, fs-io, fs-all"),

  F5(SW_TRACE_CWAL,2,"trace-cwal", 0,
     "Enables tremendous amounts of cwal_engine "
     "tracing if it's compiled in."),
  F5(SW_SWEEP_INTERVAL,1,"w",0,"Increase sweep interval by 1."),
  F5(SW_VAC_INTERVAL,-1,"w",0, "Increase vacuum interval by 1."),
  F01(SW_TRACE_STACKS_0,"T","stack machine tracing."),
  F01(SW_TRACE_SWEEP_0,"W","sweep tracking."),
  FCONTINUE("Enable multiple times for (possibly) more detail."),

  FINFOLINE("Recycling-related options:"),
  F01(SW_RE_V0,"R","value recycling."),
  F01(SW_RE_S0,"S","string interning."),
  F01(SW_RE_C0,"C","the chunk recyler."),

  FINFOLINE("Memory-tracking/capping options:"),
  
  F6(SW_MCAP_TOTAL_ALLOCS, 1, "cap-ta", "N",
     "* Limit the (T)otal number of (A)llocations to N.",
     CLIAPP_F_SPACE_VALUE),
  F6(SW_MCAP_TOTAL_ALLOCS, 1, "cap-tb", "N",
     "* Limit the (T)otal number of allocated (B)ytes to N.",
     CLIAPP_F_SPACE_VALUE),
  F6(SW_MCAP_TOTAL_ALLOCS, 1, "cap-ca", "N",
     "Limit the (C)oncurrent number of (A)llocations to N.",
     CLIAPP_F_SPACE_VALUE),
  F6(SW_MCAP_TOTAL_ALLOCS, 1, "cap-cb", "N",
     "Limit the (C)oncurrent number of allocated (B)ytes to N.",
     CLIAPP_F_SPACE_VALUE),
  F6(SW_MCAP_TOTAL_ALLOCS, 1, "cap-sb", "N",
     "Limit the size of any (S)ingle allocation to N (B)ytes.",
     CLIAPP_F_SPACE_VALUE),
  FINFOLINE("The allocator starts failing (returning NULL) when a capping constraint is violated."),
  FCONTINUE("The 'tb' and 'ta' constraint violations are permanent (recovery requires resetting the engine)."),
  FCONTINUE("The 'ca' and 'cb' constraints are recoverable once some cwal-managed memory gets freed."),
  FCONTINUE("Capping only applies to memory allocated by/via the scripting\n"
            "engine, not 3rd-party APIs."),

  F01(SW_MEM_FAST0, "fst", "(F)orced alloc (S)ize (T)racking."),
  FCONTINUE("Enabled implicitly by some -cap flags."),

#undef GROUP
#undef F01
#undef F5
#undef F6
#undef FX
#undef FPLUS
#undef FPLUSX
#undef FINFOLINE
  CliAppSwitch_sentinel /* MUST be the last entry in the list */

  };
  return cliAppSwitches;
}
