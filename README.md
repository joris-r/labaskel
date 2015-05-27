Labaskel
========

The name can stand for (you chose)

- "LAboratory for the B method in hASKELl"
- "La méthode B en hASKELl"


This tool parse the "B Method" language in Haskell.
Nothing more right now.

# What's done

- basic pretty-printer
- generating random BTree with Quickcheck
- testing with Quickcheck (printing, parsing and printing again must give an equivalent BTree)
- try to do LALR grammar with alex/happy (processind ambiguities is problematic)
- do a combinatory parser with the Parsec library (wihtout ambigious stuff)
- add a state to the parser in order to accept pair with comma
- parse ambigious "{ ... }" construct
- force block around semi-colon sequences
- force parenthesis around ";" and "||" relation operators


# What's needed (bug or shortcoming)

- better pretty-printing (only necessary parenthesis) for better testing
- fix the buggy tokenizer
- check if the renaming in identifier can be chained "a.b.c"
- parse chained fonction application "f(1)(2)"
- rename Util module
- still several factorization in BExpression

# What should
- check mandatory parenthesis for predicate negation
- add a command line executable to pretty-print source file
- the "not" is parsed as an unary operator ?

# What could be done
- try uniplate to add parenthesis
- add an XML output and test the program versus the XML output of BComp on some projects
- remove things from the BTree that could be defined regulary (like a std lib)
- use this shrinking stuff of quickcheck
- process B definitions
- add payload data to the BTree
- record source location while parsing
- type inference
- proof obligation generation
- a nice pretty-printing with smart indentation
- refactoring capability
- add the endless stream of verifications for architecture, visibility, etc 


# Arguments for the language choice

Must have:

- strong static typing (for safety)
- automatic memory management (for safety, and speed of development)
- multi-threading support
- safe design (runtime error must be avoided)
- several compilers available (possibility to do a double chain of executables)
- can do native compiling (for performance and easier distribution)
- not too exotic (community, support, libraries, documentation and books)
- expressive and abstract (at least pattern matching support)
- stable language (if possible with an approved standard)
- good and stable standard library
- good libraries to program languages tools and compilers
- free and open-source compilers and libraries
