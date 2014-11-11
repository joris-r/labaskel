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


# What could be done

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
- safe design (runtime error must be avoided)
- several compilers available (possibility to do a double chain of executables)
- can do native compiling (for performance and easier distribution)
- not too exotic (community, support, libraries, documentation and books)
- expressive and abstract (at least pattern matching support)
- stable language (if possible with an approved standard)
- good and stable standard library
- good libraries to program languages tools and compilers
- free and open-source compilers and libraries
