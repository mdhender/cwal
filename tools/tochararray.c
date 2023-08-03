/**
   A quick hack to convert the contents of stdin to a C char array for
   embedding file content in C code.  It sends all output to stdout.

   Usage: $0 funcName < input > output

   It outputs a static const C byte array and a non-static function to
   access the bytes and length of the array.
*/
#include <stdio.h>

struct {
    char const * funcName;
    FILE * input;
    FILE * output;
} App = {
"MyArray" /*funcName*/,
0/*input*/,
0/*output*/
};

static void do_it_all(){
    int const splitAt = 16;
    unsigned count = 0;
    int ch;
    char const * indent = "  ";
    FILE * out = App.output;
    char const * fname = App.funcName;
    fprintf(out,"static unsigned char const %s_data[] = {\n%s",
            fname, indent);
    while( EOF != (ch = fgetc(App.input))){
        if(count){
            fprintf(out, ", ");
        }
        if(!(count++ % splitAt) && count>1){
            fprintf(out, "\n%s", indent);
        }
        fprintf(out, "%d"/*"0x%02x" decimal is smaller*/, ch);
    }
    /* fprintf(App.output,", 0"); */
    fprintf(out,", 0/*\"just in case\"*/\n}/* %s_data */;\n\n", fname);
    fprintf(out,
            "unsigned char const * %s( unsigned int * len ){\n"
            "  if(len) *len = (unsigned)sizeof(%s_data)/sizeof(%s_data[0])"
            "-sizeof(%s_data[0]) /* injected NUL byte */;\n"
            "  return &%s_data[0];\n"
            "}\n",
            fname, fname, fname, fname, fname);


    fprintf(out, "\n/* For use in client code: */\n");
    fprintf(out, "/**\n  Returns the raw static bytes of %s. If len is\n"
            "  not NULL then *len is set to its length, in bytes.\n"
            "*/\n", fname);
    fprintf(out,"unsigned char const * %s( unsigned int * len );\n",
            fname);
}

int main( int argc, char const * const * argv ){
    if(argc<2){
        fprintf(stderr, "Usage: %s functionName < input > output\n", argv[0]);
        return 1;
    }
    App.funcName = argv[1];
    App.input = stdin;
    App.output = stdout;
    do_it_all();
    return 0;
}
