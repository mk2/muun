**ONGOING PROJECT, NOT COMPLETED**

# muun
Scripting Programming Language for my study

# EBNF (based on ANTLR lua grammer [https://github.com/antlr/grammars-v4/blob/master/lua/Lua.g4](https://github.com/antlr/grammars-v4/blob/master/lua/Lua.g4))
## change points from lua
- The grammer is to be represented with LL(1) parsing.
- The grammer is easy to implement for its virtual machine.

```EBNF
chunk ::= block

block ::= {stmt [`:']} [laststmt [`;']]

stmt ::=  varlist `:=' explist |
          funcall |
          muun_label |
          `do' block `end' |
          `while' exp `do' block `end' |
          `repeat' block `until' exp |
          `if' exp `then' block {`elseif' exp `then' block} [`else' block] `end' |
          `for' Name `=' exp `,' exp [`,' exp] `do' block `end' |
          `for' namelist `in' explist `do' block `end' |
          `fun' funname funbody |
          `let' `fun' Name funbody |
          `let' namelist [`:=' explist]

laststmt ::= `return' [explist] | `break' | muun_goto

muun_goto ::= `goto' Name

muun_label ::= `::' Name `::'

funname ::= Name {`.' Name} [`:' Name]

varlist ::= var {`,' var}

var ::=  Name | prefixexp `[' exp `]' | prefixexp `.' Name

namelist ::= Name {`,' Name}

explist ::= {exp `,'} exp

exp ::=  `nil' | `false' | `true' | numeric | String | `...' | fun |
         prefixexp | tableconstructor | exp binop exp | unop exp

numeric ::= FFI_Int64 | FFI_Uint64 | FFI_Imaginary | Number

prefixexp ::= var | funcall | `(' exp `)'

funcall ::=  prefixexp args | prefixexp `:' Name args

args ::=  `(' [explist] `)' | tableconstructor | String

fun ::= `fun' funbody

funbody ::= `(' [parlist] `)' block `end'

parlist ::= namelist [`,' `...'] | `...'

tableconstructor ::= `{' [fieldlist] `}'

fieldlist ::= field {fieldsep field} [fieldsep]

field ::= `[' exp `]' `:=' exp | Name `:=' exp | exp

fieldsep ::= `,' | `;'

binop ::= `+' | `-' | `*' | `/' | `^' | `%' | `..' |
          `<' | `<=' | `>' | `>=' | `==' | `!=' |
          `and' | `or'

unop ::= `-' | `not' | `#'
```
