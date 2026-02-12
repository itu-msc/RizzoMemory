
Globals/top level

```
let x = e
fun x params = e
```

expressions
```
e ::= 
| x --variables
|  e es --application
| (e,e)
| e :: e   --Binary SigCons
| match e with es
| fun xs -> e
| let x = e in e
```


