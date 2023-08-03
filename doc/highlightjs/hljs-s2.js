/**
   A basic hljs language definition for s2.

   Intended to be included via the fossil site's skin header
   after highlight.js has been loaded:

HighlightJS:
<script src="$baseurl/raw?ci=trunk&filename=doc/highlightjs/highlight.pack.js"></script>

This file:
<script src="$baseurl/raw?ci=trunk&filename=doc/highlightjs/hljs-s2.js"></script>


Missing bits: heredocs. We can't define a closing token without
knowing the opening token, so it would seem to be (but not sure)
impossible to formulate the syntax for a heredoc here. The bash
language mode does no special handling of heredocs, presumably for
that reason.
*/
if('undefined'!==typeof hljs){
(function(hljs){
    //hljs.registerLanguage("s2", ()=>hljs.getLanguage("actionscript")); // actionscript works better than js for s2
    // ^^^ actionscript is a close match for s2.
    const kwd = [
        "__BREAKPOINT", "__COLUMN", "__FILE", "__FILEDIR",
        "__FLC", "__LINE", "affirm", "assert",
        "break", "catch", "class", "const",
        "continue", "defined", "delete", "do",
        "echo", "enum", "eval", "exception",
        "exit", "fatal", "for",
        "foreach", "function", "if", "import",
        "include", "inherits", "interface", "is",
        "isa", "nameof", "new",
        "pragma",
        "private", "proc", "protected", "public",
        "refcount", "return", "s2out", "scope",
        "static", "throw", "try",
        "typeinfo", "typename", "unset",
        "using", "var", "while"
        // ^^^ note that some are missing here, defined as literals below...
    ];
    const js = hljs.getLanguage('javascript');
    hljs.registerLanguage('s2', function(hljs){
        return {
            disableAutodetect: true,
            case_insensitive: false,
            className: 's2',
            keywords: {
                keyword: kwd.join(' '),
                literal: [
                    "false", "true", "null", "undefined"
                ].join(' ')
            },
            contains:
            [
                hljs.C_LINE_COMMENT_MODE,
                hljs.C_BLOCK_COMMENT_MODE,
                {
                    className: 'string',
                    contains: [hljs.BACKSLASH_ESCAPE],
                    relevance: 0,
                    variants: [
                        {begin: /"/, end: /"/},
                        {begin: /'/, end: /'/, relevance: 1}
                    ]
                }
            ]
        }
    });
})(hljs);
}

