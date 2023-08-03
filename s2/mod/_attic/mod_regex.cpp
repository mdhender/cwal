#include <string>
#include <regex>

#include "s2_amalgamation.h"
/*
  gcc -c -fPIC -I../include -I. -std=gnu++11 mod_regex.cpp
  gcc -fPIC -shared -rdynamic -o mod_regex.so mod_regex.o -lstdc++

s2:
  var mod = s2.loadModule('mod_regex.so',{})
*/
static int regex_match(cwal_callback_args const *args, cwal_value **rv) {
    if (args->argc < 2) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Usage: Regex.Match(subject, regex)");

    // subject
    char const * subject = cwal_value_get_cstr(args->argv[0], 0);
    char const * regex = cwal_value_get_cstr(args->argv[1], 0);

    if (!subject || !regex) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Expecting 2 string values.");

    try{
        // compiled expression
        std::regex re(regex);

        // find matches
        std::smatch sm;
        std::string strSubject = subject;
        if (std::regex_match(strSubject, sm, re) && sm.size() > 0) {
            cwal_array *ar = cwal_new_array(args->engine);
            cwal_array_length_set(ar, sm.size());
            for (unsigned i = 0; i < sm.size(); ++i) {
                cwal_array_set(ar, i, cwal_new_string_value(args->engine, sm[i].str().c_str(), 0));
            }
            *rv = cwal_array_value(ar);
        } else {
            *rv = cwal_value_null();
        }
        return 0;
    }catch(std::exception const &ex){
        return cwal_exception_setf(args->engine,
                                   CWAL_RC_EXCEPTION,
                                   "Caught std::exception: %s", ex.what());
    }
}

static int regex_search(cwal_callback_args const *args, cwal_value **rv) {
    if (args->argc < 2) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Usage: Regex.Search(subject, regex)");
    
    // subject
    cwal_size_t len = 0;
    char const * subject = cwal_value_get_cstr(args->argv[0], 0);
    char const * regex = cwal_value_get_cstr(args->argv[1], &len);
    if (!subject || !regex || !len){
        return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Expecting (string, regexString) values.");
    }

    try{

        // compiled expression
        std::regex re(regex);

        // find matches
        std::smatch sm;
        std::string strSubject = subject;
        if (std::regex_match(strSubject, sm, re) && sm.size() > 0) {
            *rv = cwal_new_integer(args->engine, sm.position());
        } else {
            *rv = cwal_new_integer(args->engine, -1);
        }
        return 0;
     }catch(std::exception const &ex){
        return cwal_exception_setf(args->engine,
                                   CWAL_RC_EXCEPTION,
                                   "Caught std::exception: %s", ex.what());
    }
       
}

static int regex_replace(cwal_callback_args const *args, cwal_value **rv) {
    if (args->argc < 3) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Usage: Regex.Replace(subject, regex, replacement)");
    
    char const * subject = cwal_value_get_cstr(args->argv[0], 0);
    char const * replacement = cwal_value_get_cstr(args->argv[2], 0);
    char const * regex = cwal_value_get_cstr(args->argv[1], 0);
    if (!subject || !replacement || !regex){
        return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Expecting 3 string values.");
    }

    try{
        // compiled expression
        std::regex re(regex);

        // replace matches
        std::string strSubject = subject;
        std::string strReplacement = replacement;
        std::string result;
        std::regex_replace(std::back_inserter(result), strSubject.begin(), strSubject.end(), re, strReplacement);
        *rv = cwal_new_string_value(args->engine, result.c_str(), (cwal_size_t)result.size());
        return 0;
    }catch(std::exception const &ex){
        return cwal_exception_setf(args->engine,
                                   CWAL_RC_EXCEPTION,
                                   "Caught std::exception: %s", ex.what());
    }
}

static int regex_split(cwal_callback_args const *args, cwal_value **rv) {
    if (args->argc < 2) return cwal_exception_setf(args->engine, CWAL_RC_MISUSE, "Usage: Regex.Split(subject, regex)");
    *rv = cwal_value_null();
	return 0;
}

static int regex_init(s2_engine *se, cwal_value *ns) {
	// Return the Regex Functions
	s2_func_def const funcs[] = {
		S2_FUNC2("Match", regex_match),
		S2_FUNC2("Search", regex_search),
		S2_FUNC2("Replace", regex_replace),
		S2_FUNC2("Split", regex_split),
		s2_func_def_empty_m
	};
	s2_install_functions(se, ns, funcs, 0);
	return 0;
}

S2_MODULE_IMPL(regex, regex_init);
S2_MODULE_IMPL1(regex, regex_init);
