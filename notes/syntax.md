
Globals/top level

```
let x = e
fun x params = e
```

expressions
```
e ::= 
| x --variables
| e es --application
| (e,e)
| e :: e   --Binary SigCons
| e |> e --binary pipe (>)
| match e with |? <case> -> expresion | ... -- patteren matching, | can be on new lines
| fun xs -> e
| let x = e in e
```

Comments using `//` for single line and `/* */` for multi-line

```
let <name> = expresion
fun <funName> <params ...> = { expresion } -- Implicit return of value on last line
fun <params ...> -> expresion -- lambda functions
<funName> <param> <param> ... -- applying a function
```

type (annotations). Captitalized, because ?
```
Int -> Int -> Int   -- functions with '->'
Signal A            -- polymorphic types go left-to-right
A * B               -- tuples?

(e: type)           -- annotations?
```