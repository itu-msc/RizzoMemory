# Understanding matching

Runtime keeps track of tags for constructor values. A header is conceptually a tuple:
`header`: $\langle \text{tag}, \text{refcount}, \text{fields} \dots \rangle$

```rizz
match mysync with
| Left a     -> Some (a+1)
| Right b    -> Some (b+10)
| Both (a,b) -> Some (a + b)
```

If you were write this program in the immutable beans calculus, it look as below:

```js
case mysync <2> of [
    Left<1> (let a = proj0 mysync in ctor1(a+1))
    Right<2>(let b = proj0 mysync in ctor2(b+10))
    Both<3> (let a = proj0 mysync in let b = proj1 mysync in ctor3(a+b))
]
```

`Left<1>` etc. is the name of the constructor + its tag. The `<2>` on the scrutinee is for the example and represents the value of mysync at execution time.

Nested (constant) patterns. Consider the Rizzo program

```rizz
match mysig with
| 5 :: tl_5 -> ...
| 7 :: tl_7 -> ...
| hd :: tl -> ...
```

We could translate the nested constant patterns into a series of if-then-else.

```js
let hd = proj0 mysig in 
case hd == 5 of [
    True<1>(let tl_5 = proj1 mysig in ...)
    False<2>(
        case hd == 7 of [
        True<1>(let tl_7 = proj1 mysig in ...)
        False<2>(let tl = proj1 mysig in ...)
    ])
]
```

If at runtime the expression `head == 5` evaluates to a `True<1>` then the first branch of the outer case is taken (equivalent to executing the `5 :: tl_5` branch).
