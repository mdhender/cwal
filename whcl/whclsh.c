/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=4 et sw=2 tw=80: */
#include "libwhcl.h"
#include "cliapp.h"

#define WHCL_OS_UNIX /*FIXME: set this via generatable/ediable config */
#if defined(WHCL_OS_UNIX)
#  include <unistd.h> /* isatty() and friends */
#endif
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <locale.h> /* setlocale() */
#include <assert.h>

/*
  MARKER((...)) is an Internally-used output routine which
  includes file/line/column info.
*/
#define MARKER(pfexp) \
  if(1) cliapp_print("%s:%d:  ",__FILE__,__LINE__); \
  if(1) cliapp_print pfexp
#define MESSAGE(pfexp) cliapp_print pfexp
#define WARN(pfexp) \
    if(1) cliapp_warn("%s:  ",App.appName); \
    if(1) cliapp_warn pfexp
#define VERBOSE(LEVEL,pfexp) if(App.verbosity >= LEVEL) { cliapp_print("verbose: "); MESSAGE(pfexp); }(void)0

/**
   WHCLSH_MAX_SCRIPTS = max number of scripts to run, including the -e
   scripts and the main script filename, but not the autoload script.
*/
#define WHCLSH_MAX_SCRIPTS 10

/**
   Global app-level state.
*/
static struct {
  char const * appName;
  int enableValueRecycling;
  /**
     Max chunk size for mem chunk recycler. Set to 0 to disable.  Must
     allocate sizeof(void*) times this number, so don't set it too
     high.
  */
  cwal_size_t maxChunkCount;
  int enableStringInterning;
  int showMetrics;
  int verbosity;
  char const * inFile;
  char const * outFile;
  char const * const * scriptArgv;
  int scriptArgc;
  int sweepInterval;
  int vacuumInterval;
  /**
     Name of interactive most history file. Currently
     looks in/writes to only the current dir.
  */
  char const * editHistoryFile;
  /**
     Default prompt string for the line-reading functions.
  */
  char const * lnPrompt;
  /**
     0=non-interactive mode, >0=interactive mode, <0=as-yet-unknown.
  */
  int interactive;
  /**
     This is the "s2" global object. It's not generally kosher to hold
     global cwal_values, but (A) we're careful in how we do so and (B)
     it simplifies a couple of our implementations.
  */
  cwal_value * s2Global;
  /**
     When shellExitFlag!=0, interactive readline looping stops.
  */
  int shellExitFlag;

  /**
     A flag used to tell us not to save the history - we avoid saving
     when the session has no commands, so that we don't get empty
     history files laying around. Once whclsh_history_add() is called,
     this flag gets set to non-0.

  */
  int saveHistory;

  /**
     A flag to force installation of the whcl shell API, even in
     non-interactive mode.
  */
  short installShellApi;

  short traceAssert;

  bool showSizeofs;

  /**
     If true, only the core language gets installed, without all of
     the class-level infrastructure.
  */
  bool cleanroom;

  /**
     If true, the bootstrapping process will look for a file named by
     the WHCLSH_INIT_SCRIPT environment variable and load it. If the
     file is not found, that's not an error, but it loading/eval'ing
     it fails then whclsh startup will fail.
  */
  bool enableInitScript;
  
  /** See whcl_feature_flag_set() WHCL_FEATURE_F_DEBUG_COMPILE. */
  bool debugCompile;

  /** Gets set to true main() if it thinks we're on a TTY. */
  bool isATTY;

  /**
     Memory-capping configuration. Search this file for App.memcap for
     how to set it up.
  */
  cwal_memcap_config memcap;
  
  /**
     Scripts for the -e/-f flags.
  */
  struct {
    int nScripts /*current # of entries in this->e*/;
    struct {
      /* True if this->src is a filename, else it's script code. */
      int isFile;
      /* Filename or script code. */
      char const * src;
    } e[WHCLSH_MAX_SCRIPTS];
  } scripts;
  int32_t cwalTraceFlags;
} App = {
0/*appName*/,
1/*enableValueRecycling*/,
50/*maxChunkCount*/,
1/*enableStringInterning*/,
0/*showMetrics*/,
0/*verbosity*/,
0/*inFile*/,
0/*outFile*/,
0/*scriptArgv*/,
0/*scriptArgc*/,
20/*sweepInterval*/,
20/*vacuumInterval*/,
"whclsh.history"/*editHistoryFile*/,
"whclsh> "/*lnPrompt*/,
-1/*interactive*/,
0/*s2Global*/,
0/*shellExitFlag*/,
0/*saveHistory*/,
-1/*installShellApi*/,
0/*traceAssert*/,
false/*showSizeofs*/,
false/*cleanroom*/,
true/*enableInitScript*/,
false/*debugCompile*/,
false/*isATTY*/,
cwal_memcap_config_empty_m/*memcap*/,
{0/*nScripts*/, {/*e*/{0,0}}},
0/*cwalTraceFlags*/
};


#if 0
static cwal_engine * printfEngine = 0;
static void whclsh_print(char const * fmt, ...) {
  va_list args;
  va_start(args,fmt);
  if(printfEngine){
    cwal_outputfv(printfEngine, fmt, args);
  }else{
      cliapp_print(fmt,args);
  }
  va_end(args);
}
#endif

/**
   ID values and flags for CLI switches.
*/
enum whclsh_switch_ids {
/** Mask of the bits used as flag IDs. The remainder are flag/modifier
    bits. */
SW_ID_MASK = 0xFFFF,
SW_ID_MASK_bits = 16,
#define ID(X) (X<< SW_ID_MASK_bits)

/**
   An entry flagged as SW_INFOLINE is intended to be a standalone line
   of information unrelated to a specific flag. If it needs to span
   lines, it must embed \n characters. --help outputs its "brief"
   member.
*/
SW_INFOLINE = ID(1),
/**
   An entry flagged with SW_CONTINUE must immediately follow either an
   SW_INFOLINE, a switch entry (SW_ID_MASK), or another entry with
   this flag. --help outputs its "brief" member, indented or not,
   depending on the previous entry.
*/
SW_CONTINUE = ID(2),

/**
   SW_GROUP_x are used for grouping related elements into tiers.
*/
SW_GROUP_0 = ID(4),
SW_GROUP_1 = ID(8),
SW_GROUP_2 = ID(0x10),
SW_GROUPS = SW_GROUP_0 | SW_GROUP_1 | SW_GROUP_2,

/**
   Switches with this bit set are not output by --help.
   (Workaround for whclsh -help/--help/-? having 3 options!)
*/
SW_UNDOCUMENTED = ID(0x20),

#undef ID

SW_switch_start = 0,
/*
  Entries for individual CLI switches...

  DO NOT bitmask the SW_GROUP entries directly with the
  switch-specific entries. They are handled in a separate step. Long
  story.

  The SW_ID_MASK bits of the switch ID values need not be bitmasks.
  i.e. they may have bits which overlap with other entries, provided that
  the SW_ID_MASK part is unique.

  All switches which *require* a value, as opposed to optionally
  accept a value, should, for consistency, have their pflags member
  OR'd with CLIAPP_F_SPACE_VALUE.

  All switches which may only be provided once must have their pflags
  field OR'd with CLIAPP_F_ONCE.
*/
SW_CLEANROOM,
SW_DEBUG_COMPILE,
SW_ESCRIPT,
SW_HELP,
SW_INFILE,
SW_INTERVAL_SWEEP,
SW_INTERVAL_VAC,
SW_INTERACTIVE,
SW_METRICS,
SW_OUTFILE,
SW_SHOW_SIZEOFS,
SW_VERBOSE,
SW_VERSION,

/**
  The entries ending with 0 or 1 are toggles. The #1 part of each pair
  MUST have a value equal to the corresponding #0 entry plus 1.
*/
SW_MEM_FAST0, SW_MEM_FAST1,
/** SW_RE_xxx = recycling-related options */
/* [no-]recycle-chunks */
SW_RE_C0, SW_RE_C1,
/* [no-]string-interning */
SW_RE_S0, SW_RE_S1,
/* [no-]recycle-values */
SW_RE_V0, SW_RE_V1,
/* --s2.shell */
SW_SHELL_API_1,

/* -I/--no-init-script */
SW_INIT_SCRIPT_0,

SW_ASSERT_0, SW_ASSERT_1,
SW_HIST_FILE_0, SW_HIST_FILE_1,
SW_TRACE_CWAL,


/* Memory-capping options... */
SW_MCAP_TOTAL_ALLOCS,
SW_MCAP_TOTAL_BYTES,
SW_MCAP_CONC_ALLOCS,
SW_MCAP_CONC_BYTES,
SW_MCAP_SINGLE_BYTES,

SW_end /* MUST be the last entry */
};

#if !defined(WHCLSH_EXTEND_FUNC_NAME)
#  define WHCLSH_EXTEND_FUNC_NAME whclsh_extend
#endif
/*
  If WHCLSH_EXTEND is defined, the client must define
  whclsh_extend() (see whclsh_extend.c for one implementation) and
  link it in with this app. It will be called relatively early in the
  initialization process so that any auto-loaded scripts can make use
  of it. It must, on error, return one of the non-0 CWAL_RC_xxx error
  codes (NOT a code from outside the range! EVER!). An error is
  treated as fatal to the shell.

  See whclsh_extend.c for a documented example of how to use this
  approach to extending this app.

*/
#if defined(WHCLSH_EXTEND)
/**
   Clients should implement any extensions to the whclsh interpreter
   here.
   
   argc and argv contain all arguments passed to main(). el is the
   whcl interpreter used by the application.

   Client code may use whcl_scope_current() or
   cwal_scope_current_get() (via whcl_engine_cwal()) to get the
   current scope, and may install functionality there if they
   like. More simply, whcl_set() and friends can be used to add
   features to the current scope. whclsh calls this function only from
   the global scope, so any features added to the current scope will,
   if they're not manually removed, live as long as the interpreter
   instance.

   mainNamespace is whclsh's "whcl" object. In practice, that is where
   most functionality gets installed. Note that we're not limited to
   installing in one specific place, but in practice that is typical.
   That value is guaranteed by whcl to live until the top-most scope
   is cleaned up (when the shell shuts down) unless the client
   _intentionally_ does something silly from the C level and somehow
   destroys that value.

   Must return 0 on success or one of the CWAL_RC_xxx values on
   error. On error, the application must treat it as fatal to the
   scripting engine. The default shell ends the application
   (gracefully, as opposed to abort()) if this function returns non-0.
*/
extern int WHCLSH_EXTEND_FUNC_NAME(whcl_engine * const el,
                                   cwal_value * const mainNamespace,
                                   int argc, char const * const * argv);
#endif


static CliAppSwitch const * whclsh_cliapp_switches(){
  static const CliAppSwitch cliAppSwitches[] = {
  /* {opaque, dash, key, value, brief, details, callback, reserved} */
  /** Fills out most of a CliAppSwitch entry */
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
#define F01(IDFLAGS,SHORT,LONG,BRIEF)                       \
  {IDFLAGS+1, 2, LONG,   0, "Enable "  BRIEF, 0, 0, 0},     \
  {IDFLAGS+1, 1, SHORT,  0, "Enable "  BRIEF, 0, 0, 0},     \
  {IDFLAGS,   2, "no-"LONG, 0, "Disable " BRIEF, 0, 0, 0},  \
  {IDFLAGS,   1, "no"SHORT, 0, "Disable " BRIEF, 0, 0, 0}
  /** Start the next group (for --help grouping). */
#define GROUP(GID,DESC) {GID, 0, 0, 0, DESC, 0, 0, 0}

  /***********************************************************************/
  GROUP(SW_GROUP_0,0),

  /**
     Keep these arranged so that flags with short and long forms are
     ordered consecutively, long one first. The help system uses that to
     collapse the flags into a single listing. In such cases, the help
     text from the first entry is used and the second is ignored.
  */
  FX(SW_HELP, "?", "help", 0,
     "Send help text to stdout and exit with code 0.", 0),

  FX(SW_VERBOSE,"v","verbose",0,
     "Increases verbosity level by 1.", 0),

  FX(SW_ESCRIPT,"e","eval","SCRIPT",
     "Evaluates the given script code in the global scope. "
     "May be used multiple times.", CLIAPP_F_SPACE_VALUE),

  FX(SW_INFILE,
     "f","file","infile",
     "Evaluates the given file. The first non-flag argument is used "
     "by default.", CLIAPP_F_SPACE_VALUE | CLIAPP_F_ONCE),

  FX(SW_OUTFILE, "o", "output", "outfile",
     "Sets cwal's output channel to the given file. Default is stdout.",
     CLIAPP_F_SPACE_VALUE | CLIAPP_F_ONCE),

  FX(SW_DEBUG_COMPILE, "D", "debug", 0,
     "Enables processing of __debug {...} blocks in scripts.", 0),
  
  /***********************************************************************/
  GROUP(SW_GROUP_1, "Esoteric options:"),


  FX(SW_ASSERT_1,"A", "assert", 0,
     "Increase assertion trace level by one. May be used multiple times: "
     "1=trace assert, 2=also trace affirm.",0),

  F5(SW_INTERACTIVE,1,"i",0,
     "Enter interactive mode even if -e/-f disable it."),

  F6(SW_CLEANROOM,2,"cleanroom",0,
     "Only core language functionality with no "
     "class-/prototype-level methods.",0),

  F6(SW_SHELL_API_1,2,"whcl.shell",0,
     "Forces installation of the whcl[shell] API unless trumped by --cleanroom.",0),
  FCONTINUE("By default whcl[shell] is only installed in interactive mode."),

  FX(SW_INIT_SCRIPT_0, "I", "no-init-script", 0,
     "Disable loading of $WHCLSH_INIT_SCRIPT.", 0),
  
  F6(SW_HIST_FILE_1,1,"h","filename", "Set editing history file.",
     CLIAPP_F_SPACE_VALUE | CLIAPP_F_ONCE),
  F5(SW_HIST_FILE_0,2,"no-history",0, "Disable editing history file."),

  FX(SW_SHOW_SIZEOFS, "z", "sizeof", 0,
     "Show various sizeof() values.", 0),
  
  FX(SW_METRICS, "m", "metrics", 0,
     "Show various cwal/whcl metrics before shutdown.",0),
  //FCONTINUE("Use -v once or twice for more info."),

  FX(SW_INTERVAL_SWEEP, "is", "interval-sweep", "N",
     "Sets the sweep interval (measured in number of commands) to N.",
     CLIAPP_F_SPACE_VALUE),
  FX(SW_INTERVAL_VAC, "iv", "interval-vacuum", "N",
     "Sets every Nth sweep-up to be a vacuum.",
     CLIAPP_F_SPACE_VALUE),

  FINFOLINE("Recycling-related options:"),
  F01(SW_RE_V0,"rv", "recycle-values", "recycling of value instances."),
  F01(SW_RE_C0,"rc","recycle-chunks", "the memory chunk recycler."),
  F01(SW_RE_S0,"si","string-interning", "string interning."),

  FINFOLINE("Memory-tracking/capping options:"),
  FX(SW_MCAP_TOTAL_ALLOCS, "cap-ta", "memcap-total-allocs", "N",
     "* Limit the (T)otal number of (A)llocations to N.",
     CLIAPP_F_SPACE_VALUE),
  FX(SW_MCAP_TOTAL_ALLOCS, "cap-tb", "memcap-total-bytes", "N",
     "* Limit the (T)otal number of allocated (B)ytes to N.",
     CLIAPP_F_SPACE_VALUE),
  FX(SW_MCAP_TOTAL_ALLOCS, "cap-ca", "memcap-concurrent-allocs", "N",
     "Limit the (C)oncurrent number of (A)llocations to N.",
     CLIAPP_F_SPACE_VALUE),
  FX(SW_MCAP_TOTAL_ALLOCS, "cap-cb", "memcap-concurrent-bytes", "N",
     "Limit the (C)oncurrent number of allocated (B)ytes to N.",
     CLIAPP_F_SPACE_VALUE),
  FX(SW_MCAP_TOTAL_ALLOCS, "cap-sb", "memcap-single-alloc", "N",
     "Limit the size of any (S)ingle allocation to N (B)ytes.",
     CLIAPP_F_SPACE_VALUE),
  FINFOLINE("The allocator starts failing (returning NULL) when a capping constraint is violated."),
  FCONTINUE("The 'tb' and 'ta' constraint violations are permanent (recovery requires resetting the engine)."),
  FCONTINUE("The 'ca' and 'cb' constraints are recoverable once some cwal-managed memory gets freed."),
  FCONTINUE("Capping only applies to memory allocated by/via the scripting\n"
            "engine, not 3rd-party APIs."),

  F6(SW_MEM_FAST1,2,"fast",0,"(F)orce (A)lloc (S)ize (T)racking.",0),
  FCONTINUE("Enables more precise tracking and reuse of recycled memory."),
  FCONTINUE("Several memcap options enable --fast implicitly."),

  GROUP(SW_GROUP_2,"Evern Esotericer options:"),

  F5(SW_TRACE_CWAL,2,"trace-cwal", 0,
     "Enables tremendous amounts of cwal_engine "
     "tracing if it's compiled in."),
  
#undef GROUP
#undef F01
#undef F6
#undef FX
#undef FPLUS
#undef FPLUSX
#undef FINFOLINE
    CliAppSwitch_sentinel /* MUST be the last entry in the list */
  };
  return cliAppSwitches;
}


/**
   Parses and returns the value of an unsigned int CLI switch. Assigns
   *rc to 0 on success, non-0 on error.
*/
static cwal_size_t whclsh_parse_switch_unsigned(CliAppSwitch const * s, int * rc){
  cwal_int_t i = 0;
  if(0!=cwal_cstr_to_int(s->value, cwal_strlen(s->value), &i)){
    WARN(("-cap-%s must have a positive integer value.\n", s->key));
    *rc = CWAL_RC_MISUSE;
    return 0;
  }else if(i<0){
    WARN(("-cap-%s must have a positive value (got %d).\n", s->key, (int)i));
    *rc = CWAL_RC_RANGE;
    return 0;
  }else{
    *rc = 0;
    return (cwal_size_t)i;
  }
}


/**
   The help handler for a single CliAppSwitch. It requires that all
   switches are passed to it in the order they are defined at the app
   level, as it applies non-inuitive semantics to certain entries,
   e.g. collapsing -short and --long-form switches into a single
   entry.
*/
static int whclsh_help_switch_visitor(CliAppSwitch const *s,
                                    void * /* ==>int*  */ verbosity){
  static CliAppSwitch const * skip = 0;
  CliAppSwitch const * next = s+1
    /* this is safe b/c the list ends with an empty sentinel entry */;
  int group = 0;
  static int prevContinue = 0;
  if(s->opaque & SW_UNDOCUMENTED){
    return 0;
  }else if(skip == s){
      skip = 0;
      return 0;
  }
  switch(s->opaque & SW_GROUPS){
    case SW_GROUP_0: break;
    case SW_GROUP_1: group = 1; break;
    case SW_GROUP_2: group = 2; break;
  }
  assert(s->dash>=-1 && s->dash<=2);
  assert(group>=0 && group<3);
  /*MARKER(("group %d (%d) %s\n", group, *((int const*)verbosity), s->key));*/
  if(group>*((int const*)verbosity)){
      return CWAL_RC_BREAK;
  }
  if(s->opaque & SW_GROUPS){
    /* Use s->brief as a label for this section then return. */
    if(s->brief && *s->brief){
      char const * splitter = "===============================";
      cliapp_print("\n%s\n%s\n%s\n", splitter, s->brief, splitter);
    }
    return 0;
  }
  if((s->opaque & SW_ID_MASK) && (next->opaque==s->opaque)){
    /* Collapse long/short options into one entry. We expect the long
       entry to be the first one. */
    /* FIXME? This only handles collapsing 2 options, but whclsh
       help supports -?/-help/--help. */
      skip = next;
      cliapp_print("\n  %s%s | ", cliapp_flag_prefix(skip->dash),
                   skip->key);
  }else if(!next->key){
      skip = 0;
  }
  /*MARKER(("switch entry: brief=%s, details=%s\n", s->brief, s->details));*/
  if(s->key && *s->key){
    cliapp_print("%s%s%s%s%s%s%s\n",
                 skip ? "" : "\n  ",
                 cliapp_flag_prefix(s->dash), s->key,
                 s->value ? " = " : "",
                 s->value ? s->value : "",
                 s->brief ? "\n      ": "",
                 s->brief ? s->brief : "");
    prevContinue = s->opaque;
  }else{
    switch(s->opaque){
      case SW_CONTINUE:
        assert(s->brief && *s->brief);
        if(prevContinue==SW_INFOLINE){
          cliapp_print("%s\n", s->brief);
        }else{
          cliapp_print("      %s\n", s->brief);
        }
        break;
      case SW_INFOLINE:
        assert(s->brief && *s->brief);
        cliapp_print("\n%s\n", s->brief);
        prevContinue = s->opaque;
        break;
      default:
        prevContinue = s->opaque;
        break;
    }
  }
  return 0;
}

/**
   Pushes scr to App.scripts if there is space for it, else outputs an
   error message and returns non-0. isFile must be true if scr refers
   to a filename, else it is assumed to hold s2 script code.
*/
static int whclsh_push_script( char const * scr, int isFile ){
  const int n = App.scripts.nScripts;
  if(WHCLSH_MAX_SCRIPTS==n){
    MARKER(("WHCLSH_MAX_SCRIPTS (%d) exceeded: give fewer scripts or "
            "rebuild with a higher WHCLSH_MAX_SCRIPTS.\n",
            WHCLSH_MAX_SCRIPTS));
    return CWAL_RC_RANGE;
  }
  App.scripts.e[n].isFile = isFile;
  App.scripts.e[n].src = scr;
  VERBOSE(3,("Pushed %s #%d: %s\n", isFile ? "file" : "-e script",
             n+1, App.scripts.e[n].src));
  ++App.scripts.nScripts;
  return 0;
}

static void whclsh_help_header_once(){
  static int once = 0;
  if(once) return;
  once = 1;
  cliapp_print("whcl scripting engine shell\n");
}

static void whclsh_show_version(int detailedMode){
  whclsh_help_header_once();
  cliapp_print("cwal/whcl version: %s\n",
               cwal_build_info()->versionString);
  cliapp_print("Project site: https://fossil.wanderinghorse.net/r/cwal\n");
  if(detailedMode){
    cwal_build_info_t const * const bi = cwal_build_info();
    cliapp_print("\nlibcwal compile-time info:\n\n");
#define STR(MEM) cliapp_print("\t%s = %s\n", #MEM, bi->MEM)
    /*STR(versionString);*/
    STR(cppFlags);
    STR(cFlags);
    STR(cxxFlags);
#undef STR

#define SZ(MEM) cliapp_print("\t%s = %"CWAL_SIZE_T_PFMT"\n", #MEM, (cwal_size_t)bi->MEM)
    SZ(size_t_bits);
    SZ(int_t_bits);
    SZ(maxStringLength);
#undef SZ

#define BUL(MEM) cliapp_print("\t%s = %s\n", #MEM, bi->MEM ? "true" : "false")
    BUL(isJsonParserEnabled);
    BUL(isDebug);
#undef BUL

    cliapp_print("\n");
  }
}


static void whclsh_show_help(){
  int maxGroup = App.verbosity;
  char const * splitter = 1 ? "" :
    "------------------------------------------------------------\n";
  int n = 0;
  whclsh_show_version(0);
  cliapp_print("\nUsage: %s [options] [file | -f file]\n",
               App.appName);
  cliapp_print("%s",splitter);
  cliapp_switches_visit( whclsh_help_switch_visitor, &maxGroup );
  cliapp_print("\n%sNotes:\n",splitter); 
  cliapp_print("\n%d) Flags which require a value may be provided as (--flag=value) or "
        "(--flag value).\n", ++n);
  cliapp_print("\n%d) All flags after -- are ignored by the shell but "
               "made available to script code via the whcl.ARGV object.\n", ++n);
  cliapp_print("Minor achtung: the script-side arguments parser uses --flag=VALUE, "
               "whereas this app's flags accept (--flag VALUE)!\n");

  if(maxGroup<2){
      cliapp_print("\n%d) Some less obscure options were not shown. "
                   "Use -v once or twice with -? for more options.\n", ++n);
  }
  puts("");
}

static int whclshGotHelpFlag = 0;
static int whclsh_arg_callback_common(int ndx, CliAppSwitch const * s,
                                      CliAppArg * a){
  int const sFlags = s ? s->opaque : 0;
  int const sID = sFlags & SW_ID_MASK;
  int rc = 0;
  if(0 && a){
    MARKER(("switch callback sFlags=%06x, sID=%d, SW_ESCRIPT=%d: #%d %s%s%s%s\n",
            sFlags, sID, SW_ESCRIPT,
            ndx, cliapp_flag_prefix(a->dash), a->key,
            a->value ? "=" : "",
            a->value ? a->value : ""));
  }
  if(!a/* end of args - there's nothing for use to do in this case */){
    return 0;
  }
  switch(sID){
    case SW_ASSERT_0: App.traceAssert = 0; break;
    case SW_ASSERT_1: ++App.traceAssert; break;
    case SW_CLEANROOM: App.cleanroom = true; break;
    case SW_DEBUG_COMPILE: App.debugCompile = true; break;
    case SW_ESCRIPT:
      assert(a->value && "Should have been captured by cliapp.");
      rc = whclsh_push_script( a->value, 0 );
      if(App.interactive<0) App.interactive=0;
      break;
    case SW_HELP: ++whclshGotHelpFlag; break;
    case SW_HIST_FILE_1: cliApp.lineread.historyFile = a->value; break;
    case SW_HIST_FILE_0: cliApp.lineread.historyFile = 0; break;
    case SW_INFILE:
      assert(a->value && "Should have been captured by cliapp.");
      rc = whclsh_push_script( a->value, 1 );
      if(!rc) App.inFile = a->value;
      break;
    case SW_INIT_SCRIPT_0: App.enableInitScript = false; break;
    case SW_INTERACTIVE: App.interactive = 1; break;
    case SW_INTERVAL_SWEEP:
    case SW_INTERVAL_VAC:{
      int const i = atoi(a->value);
      if(i > 0x7FFF) {
        WARN(("--interval-sweep and --interval-vacuum must be <32k\n"));
        return CLIAPP_RC_RANGE;
      }
      if(sID == SW_INTERVAL_SWEEP) App.sweepInterval = (short)i;
      else App.vacuumInterval = (short)i;
      break;
    }
    case SW_MEM_FAST0: App.memcap.forceAllocSizeTracking = 0; break;
    case SW_MEM_FAST1: App.memcap.forceAllocSizeTracking = 1; break;
    case SW_MCAP_TOTAL_ALLOCS:
      App.memcap.maxTotalAllocCount =
        whclsh_parse_switch_unsigned(s, &rc);
      break;
    case SW_MCAP_TOTAL_BYTES:
      App.memcap.maxTotalMem = whclsh_parse_switch_unsigned(s, &rc);
      break;
    case SW_MCAP_CONC_ALLOCS:
      App.memcap.maxConcurrentAllocCount =
        whclsh_parse_switch_unsigned(s, &rc);
      break;
    case SW_MCAP_CONC_BYTES:
      App.memcap.maxConcurrentMem = whclsh_parse_switch_unsigned(s, &rc);
      break;
    case SW_MCAP_SINGLE_BYTES:
      App.memcap.maxSingleAllocSize =
        whclsh_parse_switch_unsigned(s, &rc);
      break;
    case SW_METRICS: App.showMetrics = 1; break;
    case SW_OUTFILE: App.outFile = a->value; break;
    case SW_RE_C0: App.maxChunkCount = 0; break;
    case SW_RE_C1: App.maxChunkCount = 50; break;
    case SW_RE_S0: App.enableStringInterning = 0; break;
    case SW_RE_S1: App.enableStringInterning = 1; break;
    case SW_RE_V0: App.enableValueRecycling = 0; break;
    case SW_RE_V1: App.enableValueRecycling = 1; break;
    //case SW_SHELL_API_0: App.installShellApi = 0; break;
    case SW_SHELL_API_1: App.installShellApi = 1; break;
    case SW_TRACE_CWAL:
      App.cwalTraceFlags = CWAL_TRACE_SCOPE_MASK
        | CWAL_TRACE_VALUE_MASK
        | CWAL_TRACE_MEM_MASK
        | CWAL_TRACE_FYI_MASK
        | CWAL_TRACE_ERROR_MASK;
      break;
    case SW_VERBOSE: ++App.verbosity; break;
    case SW_SHOW_SIZEOFS: App.showSizeofs = true;
      break;
    default: break;
  }
  return rc;
}

/**
   Called via cwal_engine_init() using our customized
   cwal_engine_vtab.
*/
static int e_init_engine(cwal_engine * const e, cwal_engine_vtab * const vtab){
  int32_t featureFlags = 0;
  /*MARKER(("cwal_engine_vtab::hook::on_init() callback. e=%p state=%p\n",
    (void const *)e, vtab->hook.init_state));*/
  if(vtab){/*unused*/}
  if(1){
    /*Enable tracing.*/
    int32_t flags = CWAL_TRACE_NONE;
    flags |= CWAL_TRACE_ERROR_MASK;
    flags |= App.cwalTraceFlags;
    cwal_engine_trace_flags( e, flags );
  }

  {
    /* Configure the memory chunk recycler... */
    cwal_memchunk_config conf = cwal_memchunk_config_empty;
    conf.maxChunkCount = App.maxChunkCount;
    conf.maxChunkSize = 1024 * 32;
#if 16 == CWAL_SIZE_T_BITS
    conf.maxTotalSize = 1024 * 63;
#else
    conf.maxTotalSize = 1024 * 256;
#endif
    conf.useForValues = 0 /* micro-optimization: true tends towards
                             sub-1% reduction in mallocs but
                             potentially has aggregate slower value
                             allocation for most cases */ ;
    int const rc = cwal_engine_memchunk_config(e, &conf);
    assert(!rc);
  }

  if(App.enableStringInterning){
    /* Enable auto-interning of strings:

       Costs a lot of memory but is pretty cool.
    */
    featureFlags |= CWAL_FEATURE_INTERN_STRINGS;
  }

    
  /* rc = cwal_engine_feature_flags(e,-1); */
  cwal_engine_feature_flags(e, /* rc |  */featureFlags );


#define REMAX(T,N) cwal_engine_recycle_max( e, CWAL_TYPE_ ## T, (N) )
  REMAX(UNDEF,0);
  if(App.enableValueRecycling){
    /* A close guess based on the post-20141129 model...
       List them in "priority order," highest priority last.
       Lower prio ones might get trumped by a higher prio one.
    */
    REMAX(UNIQUE,10)/* will end up being trumped by integer (32-bit)
                       or double (64-bit) */;
    REMAX(KVP,30) /* guaranteed individual recycler */;
    REMAX(WEAKREF,10) /* guaranteed individual recycler */;
    REMAX(STRING,30) /* guaranteed individual recycler */;
    REMAX(EXCEPTION,5);
    REMAX(HASH,20) /* might also include: function, native */;
    REMAX(BUFFER,20) /* might also include: object */ ;
    REMAX(XSTRING,20/*also Z-strings, might also include doubles*/);
    REMAX(NATIVE,20)  /* might also include: function, hash */;
    REMAX(DOUBLE,20)/* might also include x-/z-strings */;
    REMAX(FUNCTION,20) /* might include: hash, native */;
    REMAX(ARRAY,20);
    REMAX(OBJECT,20) /* might include: buffer */;
    REMAX(INTEGER,40) /* might include: double */;
    REMAX(TUPLE,30) /* might include: x-/z-strings, propref */;
    REMAX(PROPREF,30) /* might include: x-/z-strings, tuple */;
  }
#undef REMAX

  return 0;
}

static int whclsh_report_engine_error( whcl_engine * const el ){
  int rc = 0;
  cwal_error * const err = cwal_engine_error(el->ec);
  if(err->code){
    char const * msg = 0;
    rc = cwal_error_get(err, &msg, 0);
    if(err->line>0){
      fprintf(stderr,
              "whcl_engine says error #%d (%s)"
              " @ script [%s], line %d, column %d: %s\n",
              rc, cwal_rc_cstr(rc),
              (char const *)err->script.mem,
              err->line, err->col, msg);
    }else{
      fprintf(stderr,
              "whcl_engine says error #%d (%s)%s%s\n",
              rc, cwal_rc_cstr(rc),
              (msg && *msg) ? ": " : "",
              (msg && *msg) ? msg : "");
    }
  }
  return rc;
}

/**
   General-purpose result reporting function. rc should be the rc from
   the function we got *rv from. This function takes over
   responsibility of *rv from the caller (who should not take a
   reference to *rv). rv may be 0, as may *rv.

   After reporting the error, whcl_engine_sweep() is run, which _might_
   clean up *rv (along with any other temporary values in the current
   scope).
*/
static int whclsh_report_result(whcl_engine * const el, int rc, cwal_value **rv ){
  char const * label = "result";
  cwal_value * rvTmp = 0;
  bool showedMessage = 0;
  bool rcIsOkay = 0==rc;
  if(!rv) rv = &rvTmp;
  switch(rc){
    case 0: break;
    case CWAL_RC_RETURN:
      *rv = cwal_propagating_take(el->ec);
      assert(*rv);
      rcIsOkay = true;
      break;
    case CWAL_RC_EXCEPTION:
      label = "EXCEPTION";
      *rv = cwal_exception_get(el->ec);
      assert(*rv);
      cwal_value_ref(*rv);
      cwal_exception_set(el->ec, 0);
      cwal_value_unhand(*rv);
      break;
    case CWAL_RC_EXIT:
      rc = 0;
      *rv = cwal_propagating_take(el->ec);
      break;
    case CWAL_RC_BREAK:
      label = "UNHANDLED BREAK";
      *rv = cwal_propagating_take(el->ec);
      break;
    case CWAL_RC_CONTINUE:
      label = "UNHANDLED CONTINUE";
      *rv = 0;
      break;
    case CWAL_RC_FATAL:
      label = "FATAL";
      *rv = cwal_propagating_take(el->ec);
      assert(*rv);
      assert(el->ec->err.code);
      CWAL_SWITCH_FALL_THROUGH;
    default:
      ;
      if(whclsh_report_engine_error(el)){
        showedMessage = 1;
      }else{
        WARN(("Unusual result code (no error details): %d/%s\n",
              rc,cwal_rc_cstr2(rc)));
      }
      break;
  }
  if(*rv){
    assert((cwal_value_scope(*rv) || cwal_value_is_builtin(*rv))
           && "Seems like we've cleaned up too early.");
    if(App.isATTY && (rc || App.verbosity)){
      if(rc && !showedMessage && !rcIsOkay){
        fprintf(stderr,"rc=%d (%s)\n", rc, cwal_rc_cstr(rc));
      }
      whcl__dump_value(*rv, label, 0, 0, 0);
    }
    /* We are uncertain of v's origin but want to clean it up if
       possible, so... */
    /*
      This ref/unref combo ensures that if it's a temporary, it gets
      cleaned up by this unref, but if it's not, then we effectively
      do nothing to it (because someone else is holding/containing
      it).
    */
    cwal_refunref(*rv);
    *rv = 0;
  }
  whcl_err_reset(el);
  whcl_engine_sweep(el);
  return rc;
}

/**
   Callback for cliapp_repl().
*/
static int CliApp_repl_f_whclsh(char const * line, void * state){
  static cwal_hash_t prevHash = 0;
  cwal_size_t const nLine = cwal_strlen(line);
  if(nLine){
    whcl_engine * const el = (whcl_engine*)(state);
    cwal_value * v = 0;
    cwal_hash_t const lineHash = cwal_hash_bytes(line, nLine);
    int rc = 0;
    if(prevHash!=lineHash){
      /* Only add the line to the history if it seems to be different
         from the previous line. */
      cliapp_lineedit_add(line);
      prevHash = lineHash;
    }
    whcl_err_reset(el);
    whcl_set_interrupt_handlable( el );
    rc = whcl_eval_cstr( el, false, "shell input", line, (cwal_int_t)nLine, &v );
    whcl_set_interrupt_handlable( 0 )
      /* keeps our ctrl-c handler from interrupting line reading or
         accidentially injecting an error state into el. */;
    whclsh_report_result(el, rc, &v);
    if(App.shellExitFlag) return WHCL_RC_CLIENT_BEGIN;
  }
  return 0;
}

/**
   Enters an interactive-mode REPL loop if it can, otherwise it
   complains loudly about the lack of interactive editing and returns
   CWAL_RC_UNSUPPORTED.
*/
static int whclsh_interactive(whcl_engine * const el){
  int rc = 0;
  if(!cliApp.lineread.enabled){
    rc = whcl_err_set(el, CWAL_RC_UNSUPPORTED,
                      "Interactive mode not supported: "
                      "this shell was built without "
                      "line editing support.");
    whclsh_report_result(el, rc, 0);
    return rc;
  }

  cliapp_lineedit_load(NULL) /* ignore errors */;
  cwal_outputf(el->ec, "whcl interactive shell. All commands "
               "are run in the current scope. Use your platform's EOF "
               "sequence on an empty line to exit. (Ctrl-D on Unix, "
               "Ctrl-Z(?) on Windows.) Ctrl-C might work, too.\n");
  {
    char const * zList = NULL;
    whcl_install_api_list(&zList, NULL);
    cwal_outputf(el->ec,
                 "Modules available for use with whcl[install-api]: %s\n",
                 zList);
  }
               
  rc = cliapp_repl( CliApp_repl_f_whclsh, &App.lnPrompt, 0, el );
  if(WHCL_RC_CLIENT_BEGIN==rc){
    /* Exit signal from the repl callback. */
    rc = 0;
  }
  cliapp_lineedit_save(NULL);
  whcl_engine_sweep(el);
  cwal_output(el->ec, "\n", 1);
  cwal_output_flush(el->ec);
  return rc;
}

static int whclsh_run_file(whcl_engine * const el, char const * filename){
  cwal_value * xrv = 0;
  int rc = whcl_eval_file(el, true, filename, &xrv);
  return whclsh_report_result(el, rc, &xrv);
}

static int whclsh_cb_exit(cwal_callback_args const * args, cwal_value ** rv){
  if(args){/*avoid unused param warning*/}
  App.shellExitFlag = 1;
  *rv = cwal_value_undefined();
  return 0;
}


#if !defined(WHCL_AMALGAMATION_BUILD)
#include "internal.h"
/**
   Test function for whcl_script_slice(). Usage:

   this-func filename [startTokenId=start-of-file [endTokenId=end-of-file]]

   It may well crash or otherwise misbehave if given token IDs out of
   range for the input file. This is a test-only function, not
   something for generic use.
*/
static int cb_test_slice( cwal_callback_args const * args,
                          cwal_value **rv ){
  int rc = 0;
  cwal_size_t fnLen = 0;
  cwal_int_t tFrom = 0, tTo = 0;
  char const * infile = args->argc
    ? cwal_value_get_cstr(args->argv[0], &fnLen)
    : 0;
  if(!infile){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a filename argument.");
  }
  if(args->argc>1){
    tFrom = cwal_value_get_integer(args->argv[1]);
    if(args->argc>2){
      tTo = cwal_value_get_integer(args->argv[2]);
    }
  }
  whcl_engine * const el = whcl_engine_from_args( args );
  assert(el);
  whcl_script * t1 = NULL;
  whcl_script * t2 = NULL;
  rc = whcl_compile_file(el, &t1, infile, NULL);
  if(rc) goto end;
  t2 = whcl__script_alloc(el->ec);
  assert(t2);
  rc = whcl__script_slice(t1,
                        tFrom>0 ? whcl__script_at(t1, (whcl_stoken_id)tFrom) : NULL,
                        tTo>0 ? whcl__script_at(t1, (whcl_stoken_id)tTo) : NULL,
                        &t2);
  whcl_script_free(t1);
  t1 = NULL;
  if(rc) goto end;
  whcl_dump_tokens(el, t2, WHCL_DUMP_TOKENS_VERBOSE
                   | WHCL_DUMP_TOKENS_METRICS);
  end:
  whcl_script_free(t2);
  *rv = cwal_value_undefined();
  return rc;
}
#endif

#if !defined(WHCL_AMALGAMATION_BUILD)
/**
   Test function for whcl_script_slice(). Usage:

   this-func filename [startTokenId=start-of-file [endTokenId=end-of-file]]

   It may well crash or otherwise misbehave if given token IDs out of
   range for the input file. This is a test-only function, not
   something for generic use.
*/
static int cb_test_xsym( cwal_callback_args const * args,
                            cwal_value **rv ){
  cwal_size_t nStr = 0;
  char const * cstr = args->argc
    ? cwal_value_get_cstr(args->argv[0], &nStr)
    : 0;
  whcl_engine * const el = whcl_engine_from_args( args );
  MARKER(("test-xsym. Flags=0x%04x\n",
          cwal_container_client_flags_get(cwal_function_value(args->callee))));
  MARKER(("test-xsym. scope flags=0x%04x\n",
          (int)el->scopes.current->flags));
  if(!cstr){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string to eval.");
  }
  return whcl_eval_cstr(el, true, "testing xsym",
                        cstr, (cwal_int_t)nStr, rv);
}
#endif /* !WHCL_AMALGAMATION_BUILD */

#if !defined(WHCL_AMALGAMATION_BUILD)
/*static*/ int cb_test_tmpl( cwal_callback_args const * args,
                            cwal_value **rv ){
  int rc = 0;
#if 0
  whcl_engine * const el = whcl_engine_from_args( args );
  cwal_engine * const e = args->engine;
  cwal_buffer code = cwal_buffer_empty;
  char const * src = ""
    ;
  cwal_buffer_append(&code, src, cwal_strlen(code));
  end:
  cwal_buffer_clear(e, &code);
#else
  if(args || rv){/*unused*/}
#endif
  return rc;
}
#endif /* !WHCL_AMALGAMATION_BUILD */


/**
   Callback implementation for whclsh_cb_history_(load|add|save)().
*/
static int whclsh_cb_history_las(cwal_callback_args const * args,
                                  cwal_value ** rv,
                                  char const * descr,
                                  int (*func)(char const * str) ){
  char const * str = args->argc
    ? cwal_value_get_cstr(args->argv[0], 0)
    : 0;
  if(!str){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting a string argument.");
  }
  else{
    int rc = (*func)(str);
    if(rc){
      rc = cwal_cb_throw(args, rc,
                       "Native %s op failed with code %d/%s.",
                       descr, rc, cwal_rc_cstr(rc));
    }else{
      *rv = cwal_value_undefined();
    }
    return rc;
  }
}

static int whclsh_cb_history_save(cwal_callback_args const * args, cwal_value ** rv){
  return whclsh_cb_history_las( args, rv, "history-save", cliapp_lineedit_save );
}

static int whclsh_cb_history_load(cwal_callback_args const * args, cwal_value ** rv){
  return whclsh_cb_history_las( args, rv, "history-load", cliapp_lineedit_load );
}

static int whclsh_cb_history_add(cwal_callback_args const * args, cwal_value ** rv){
  return whclsh_cb_history_las( args, rv, "history-add",cliapp_lineedit_add );
}


static int whclsh_cb_line_read(cwal_callback_args const * args, cwal_value ** rv){
  char const * prompt = 0;
  char * line = 0;
  if(args->argc){
    if(!(prompt = cwal_value_get_cstr(args->argv[0], 0))){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                           "Expecting a string or no arguments.");
    }
  }else{
    prompt = "prompt >";
  }
  if(!(line = cliapp_lineedit_read(prompt))){
    *rv = cwal_value_undefined();
    return 0;
  }else{
    *rv = cwal_new_string_value(args->engine, line, cwal_strlen(line));
    cliapp_lineedit_free(line);
    return *rv ? 0 : CWAL_RC_OOM;
  }
}


static int whclsh_install_shell_api( whcl_engine * const el,
                                     cwal_value * const tgt,
                                     char const * name ){
  cwal_value * sub = 0;
  int rc = 0;
  if(!cwal_props_can(tgt)) return CWAL_RC_TYPE;
  if(name && *name){
    sub = cwal_new_object_value(el->ec);
    if(!sub) return CWAL_RC_OOM;
    cwal_ref(sub);
    rc = cwal_prop_set(tgt, name, cwal_strlen(name), sub);
    cwal_unref(sub);
    if(rc) return rc;
  }else{
    sub = tgt;
  }

  {
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("tokenize-line", whcl_cb_tokenize_line),
      WHCL_FUNC2("readline", whclsh_cb_line_read),
      WHCL_FUNC2("history-save", whclsh_cb_history_save),
      WHCL_FUNC2("history-load", whclsh_cb_history_load),
      WHCL_FUNC2("history-add", whclsh_cb_history_add),
      WHCL_FUNC2("exit", whclsh_cb_exit),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, sub, funcs, 0);
  }
  return 0;
}

static int whclsh_setup_engine(whcl_engine * const el){
  int rc = 0;
  whcl_func_def funcs[] = {
  WHCL_FUNC2("exit-shell", whclsh_cb_exit),
#if !defined(WHCL_AMALGAMATION_BUILD)
  WHCL_FUNC2("test-slice", cb_test_slice),
  WHCL_FUNC2_XSYM("test-xsym", cb_test_xsym),
#endif
  whcl_func_def_empty_m
  };

  el->flags.traceAssert = App.traceAssert;
  if(App.traceAssert){
    whcl_feature_flag_set(el, WHCL_FEATURE_F_TRACE_ASSERT,
                          App.traceAssert>0);
    whcl_feature_flag_set(el, WHCL_FEATURE_F_TRACE_AFFIRM,
                          App.traceAssert>1);
  }
  whcl_feature_flag_set(el, WHCL_FEATURE_F_DEBUG_BLOCK,
                        App.debugCompile);

  /* TODO: add API for adjusting sweep/vacuum interval. */
  if(App.sweepInterval>=0){
    el->sweeper.sweepInterval = (short)App.sweepInterval;
  }
  if(App.vacuumInterval>=0){
    el->sweeper.vacuumInterval = (short)App.vacuumInterval;
  }
  
#define RC if(rc) goto end

  if(!App.cleanroom){
    rc = whcl_install_core_apis(el);
    RC;
  }
  /* Some script-implemented modules rely on ARGV for configuration,
     so it must be set up before modules are initialized. */
  rc = whcl_install_argv(el, App.scriptArgc, App.scriptArgv);
  if(0==rc && !App.cleanroom){
#if 0
    /* These are now loaded via whcl[install-api] but we may want to add a flag
       to enable them automatically, or always install them unless a certain
       flag is used. */
    if(0==rc) rc = whcl_install_fs(el);
    if(0==rc) rc = whcl_install_os(el);
    if(0==rc) rc = whcl_install_ob(el);
    if(0==rc) rc = whcl_install_json(el);
    if(0==rc) rc = whcl_install_pf(el);
#endif
    if(0==rc) rc = whcl_install_functions(el, NULL, funcs, CWAL_VAR_F_CONST);
  }
  RC;
  whcl_set_interrupt_handlable(el);

  // Much is still TODO/to port here.
  
  end:
#undef RC
  return rc;
}

static int e_main(int argc, char const * const * argv){
  whcl_engine el = whcl_engine_empty_m;
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  cwal_engine E = cwal_engine_empty;
  cwal_engine * e =
    1 ? &E : 0;
  int rc;

  setlocale(LC_CTYPE,"")
    /* supposedly important for the JSON-parsing bits and definitely
       important for the regex-posix module. */;
  if(argc||argv){/*unused*/}
  assert(cwal_output_f_FILE == vtab.outputer.output);
  assert(cwal_finalizer_f_fclose == vtab.outputer.state.finalize);
  vtab.outputer.state.data = stdout;
  vtab.tracer = cwal_engine_tracer_FILE;
  vtab.tracer.state = stdout;
  vtab.hook.on_init = e_init_engine;
  vtab.hook.init_state = e;

  if(App.outFile){
    FILE * of = (0==strcmp("-",App.outFile))
      ? stdout
      : fopen(App.outFile,"wb");
    if(!of){
      rc = CWAL_RC_IO;
      MARKER(("Could not open output file [%s].\n", App.outFile));
      goto end;
    }
    else {
      vtab.outputer.state.data = of;
      vtab.outputer.state.finalize = cwal_finalizer_f_fclose;
      vtab.tracer.state = of;
      vtab.tracer.close = NULL
        /* Reminder: we don't want both outputer and tracer
           to close 'of'. */;
    }
  }

  if(0){
    if(0) vtab.memcap.maxSingleAllocSize = 480;
    if(0) vtab.memcap.maxTotalAllocCount = 100;
    vtab.memcap.maxConcurrentMem = 1024 * 1024;
  }
#define RC if(rc) goto end;
  rc = cwal_engine_init( &e, &vtab );
  RC;
  rc = whcl_engine_init(&el, e, NULL);
  RC;

  rc = whclsh_setup_engine(&el);
  RC;

  if(!App.cleanroom){
    if(App.installShellApi>0
       || (App.interactive>0 && App.installShellApi<0)){
      rc = whclsh_install_shell_api(&el, whcl_whcl_object(&el),
                                    "shell");
      if(rc) goto end;
    }
  }
  
#if defined(WHCLSH_EXTEND)
  if(!App.cleanroom){
    if( (rc = WHCLSH_EXTEND_FUNC_NAME(&el, whcl_whcl_object(&el), argc, argv)) ) goto end;
  }else{
    VERBOSE(1,("Clean-room mode: NOT running client-defined shell extensions.\n"));
  }
#endif

  if(!App.cleanroom && App.enableInitScript){
    char const * zInit = getenv("WHCLSH_INIT_SCRIPT");
    if(zInit && *zInit){
      rc = whclsh_run_file(&el, zInit);
      if(rc){
        fprintf(stderr,"\nInit script loading failed! To disable it, "
                "use the -I/--no-init-script flags or unset the "
                "WHCLSH_INIT_SCRIPT environment variable.\n");
        goto end;
      }
    }
  }
  
  assert(App.scripts.nScripts <= WHCLSH_MAX_SCRIPTS);
  for( int i = 0; i < App.scripts.nScripts; ++i ){
    char const * script = App.scripts.e[i].src;
    /*VERBOSE(2,("Running script #%d: %s\n", i+1, script));*/
    if(!*script) continue;
    if(App.scripts.e[i].isFile){
      rc = whclsh_run_file(&el, script);
      cwal_output_flush(el.ec);
    }else{
      cwal_value * rv = 0;
      rc = whcl_eval_cstr( &el, false, "-e script",
                           script, -1,
                           App.verbosity ? &rv : NULL );
      whclsh_report_result(&el, rc, &rv);
    }
    RC;
  }

  if(App.interactive>0){
    rc = whclsh_interactive(&el);
    RC;
  }

  end:
#undef RC
  if(0==rc && App.showMetrics){
    whcl_dump_metrics(&el);
  }
  if(CWAL_RC_EXCEPTION==rc){
    cwal_value * const ex = cwal_exception_get(el.ec);
    if(ex){
      //cwal_buffer * const b = cwal_buffer_reuse(&el.escBuf);
      //cwal_buffer_format(el.ec, b, "%1$4J", -1, 1, &ex);
      //MARKER(("Exception %d: %s\n", (int)b->used, cwal_buffer_cstr(b,NULL)));
      whcl__dump_val(ex, "EXCEPTION");
    }
  }
  whcl_engine_finalize(&el);
  return rc; 
}

#if !defined(WHCL_AMALGAMATION_BUILD)
#include "tok1.h"
#include "script.h"
#include "internal.h"
#endif

void show_sizeofs(){
  MARKER(("Various library-level sizeof()s...\n"));
#define C(M) printf(#M"=%u\n", (unsigned)M)
  C(CWAL_SIZE_T_BITS);
  C(CWAL_INT_T_BITS);
  C(CWAL_VOID_PTR_IS_BIG);
#undef C
#define SO(T) printf("sizeof(%s)=%u\n", #T, (unsigned int)sizeof(T))
  SO(void*);
  SO(cwal_buffer);
  SO(cwal_engine);
  SO(cwal_int_t);
  SO(cwal_midsize_t);
  SO(cwal_scope);
  SO(cwal_size_t);
  SO(whcl_engine);
  SO(whcl_scope);
#if !defined(WHCL_AMALGAMATION_BUILD)
  SO(tok1_en);
  SO(tok1_izer);
  SO(whcl__func);
  SO(whcl_script);
  SO(whcl_stoken);
#endif
#undef SO
}

int main(int argc, char const * const * argv)
{
  int rc;
  bool gotNoop = false;

  memset(&App.scripts,0,sizeof(App.scripts));
  App.appName = argv[0];
  App.memcap.forceAllocSizeTracking = 0;
  cliApp.switches = whclsh_cliapp_switches();
  cliApp.argCallack = whclsh_arg_callback_common;
  {
    char const * zHist = getenv("WHCLSH_HISTORY_FILE");
    cliApp.lineread.historyFile =
      zHist ? zHist : App.editHistoryFile;
  }
  rc = cliapp_process_argv(argc, argv, 0);
  if(rc){
    WARN(("CLI flag processing failed: %d.\n", rc));
    rc = CWAL_RC_MISUSE
      /* It's *possibly* not a CWAL_RC_ code, to we assume misuse. */;
    goto end;
  }else if(whclshGotHelpFlag){
    whclsh_show_help();
    goto end;
  }
  App.scriptArgc = cliApp.doubleDash.argc;
  App.scriptArgv = cliApp.doubleDash.argv;

  if(App.showSizeofs){
    gotNoop = true;
    show_sizeofs();
  }else if(cliapp_arg_flag("V","version",0)){
    gotNoop = true;
    whclsh_show_version(App.verbosity);
  }else if(!App.inFile){
    /* Treat the first non-flag arg as an -f flag if no -f was
       explicitly specified. */
    CliAppArg const * ca;
    assert(argc>1 ? cliApp.cursorNonflag>0 : 1);
    ca = cliapp_arg_nonflag();
    if(ca){
      rc = whclsh_push_script( ca->key, 1 );
      if(rc) goto end;
      else App.inFile = ca->key;
    }
  }

  if(!gotNoop && App.inFile && cliapp_arg_nonflag()){
    WARN(("Extraneous non-flag arguments provided.\n"));
    rc = CWAL_RC_MISUSE;
    goto end;
  }

#ifdef WHCL_OS_UNIX
  App.isATTY = 0!=isatty(STDIN_FILENO);
#else
  App.isATTY = true /*assume*/;
#endif
  if(!gotNoop){
    if(!App.inFile && App.interactive<0
       && !App.scripts.nScripts){
#ifdef WHCL_OS_UNIX
      if(App.isATTY) App.interactive = 1;
      else App.inFile = "-";
#else
      App.inFile = "-";
#endif
    }
    rc = e_main(argc, argv);
  }
  
  end:
#if 0
  if(rc){
    MARKER(("rc=%s\n", cwal_rc_cstr(rc)));
  }
#endif
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
