# BLisp
Short for 'Basic Lisp', pronounced 'bee-lisp' so as to rhyme with 'ELisp', this is a custom Lisp dialect with a very
limited instruction set. I am not very familiar with Lisp so it may or may not end up looking anything like other Lisp
variants. The only thing that I'm certain will be like other dialects is the control flow/calculation order

## Control Flow
Each BLisp program is a single lisp statement, which simplifies the control flow. If you want multiple statements you
must construct a list from them (explained later on)

## Types
I want this language to have a strict type system, analogous to Haskell or Rust. The types I will support:
- `int` (internally stored as `i64`) is a signed int type
- `uint` (internally stored as `u64`) is an unsigned int type
- `float` (internally stored as `f64`) is a signed floating-point type
- `bool` (internally stored as `bool`) is a boolen type
- `char` (internally stored as `u8`) is an ASCII character
- `list<ty>` (internal representation tbd) is a list of type `ty` (which can be any valid type, including another list)
- `string` (internal representation tbd) is an alias for `list<char>`
- `tuple<ty1, ty2>` is a 2-tuple, where the first value is of type `ty1` and the second is of type `ty2`
- `unit`/`()` represents an empty value. Mostly used to make sequences of functions easier

### Literals

#### Integers
Any numeric literal with no period is treated as an integer literal by default. They default to `int` type unless 
`uint` is required in context. To force a literal to be `uint` append a `u` to it:
- `(1)` is `int`
- `(1u)` is `uint`
- `(-1)` is `int`
- `(-1u)` is a compilation error

#### Floats
Any numeric literal with a period is treated as a float literal. To force a numeric literal to be a float, append `f` to
it:
- `(1.)` is `float`
- `(1.0)`is `float`
- `(1f)` is `float`
- `(.1)` is a compilation error since floats must have at least one digit preceding the decimal point
- `(1.0u)` is a compilation error since floats can not be coerced to ints (unsigned or otherwise)

#### Boolean
As usual, `true` and `false` are used

#### Strings
String literals are always sequences of ASCII data enclosed by double quotes `""`. Examples:
- `("ABC")`
- `("AOTIENSR_RS_D_SD_*SRT)`
- `('ABC')` is a compilation error since double quotes were not used

#### Chars
By default, any ASCII char surrounded by single quotes '' is a char literal. You can also force a numeric literal to be
a char by appending `c`:
- `('a')` is `char`
- `("a")` is `string`
- `(57c)` is `char` and is equivalent to `('9')`
- `(256c)` is a compilation error since the max value of an ASCII char is 255 (including extended ASCII)

#### Lists
Lists are enclosed by `[]` and their type is determined by the types of the members. If the members are not all of the
same type and they can't be coerced to the same type, the list declaration is invalid:
- `[1 2 3]` is `list<int>`
- `[1u, 2, 3]` is `list<uint>` since the second and third elements are coerced to `uint` to match the first
- `[1f, 2, 3]` is `list<float>` since the second and third elements are coerced to `float` (may change)
- `[true, false]` is `list<bool>`
- `[[1u, 2, 3], [4, 5, 6]]` is `list<list<uint>>`, the second list is coerced to `list<uint>`
- `[]` is a compilation error since the list's type can't be determined without at least one value (may change later)
- `['a', "bcd"]` is a compilation error since `char` and `string` are incompatible types
- `[1, 1c]` is a compilation error because there is no implicit coercion from int literal to char (may change)

#### (2-)Tuples
Removing tuples from the spec for now. They create all sorts of annoying edge cases and aren't that useful anyway

### Type Coercion 
To allow for coercion we must define a couple abstract types and a hierarchy. To start we should look at which type 
coercions are possible, and in what contexts:
- Any numeric literal with a decimal point is a `float` and non-coercible
- Any numeric literal with a negative sign and no decimal point is `negnum` type, which defaults to `int` and can be 
coerced to `float` or `int`
- Any numeric with neither a negative sign nor a decimal point is a `num` type which defaults to `int` and can be
coerced to `int`, `uint`, or `float`
- While there is a suffix that turns integer literals into chars, at least for now there is no implicit coercion 
between the two. E.g. `[1c, 'd']` is fine but `[1c, 2]` or `['d', 2]` are errors

## Variables
Variables are values bound to a name, have an associated type, and can be used in any context that a literal of the
same type would be valid (and a few others). 

## Functions

### Math
- `+` or `add`
    - `(+ 1 2) = (add 1 2) = 3` 
    - `(+ T T) -> T` is defined for `T: int | uint | float`
- `-` or `sub`
    - `(- 2 1) = (sub 2 1) = 1`
    - `(- T T) -> T` is defined for `T: int | uint | float`
- `*` or `mul`
    - `(* 2 3) = (mul 2 3) = 6`
    - `(* T T) -> T` is defined for `T: int | uint | float`
- `/` or `div`
    - `(/ 10 2) = (div 11 2) = 5` since this would be integer division
    - `(/ 9.0 2.0) = (div 9.0 2.0) = 5.0`
    - `(* T T) -> T` is defined for `T: int | uint | float`

### I/O
- `write`
    - `(print "ABC")` prints "ABC" to stdout and returns it for convenience
    - `(print T) -> T` is defined for `T: string`
- `read`
    - `(read x)` reads a line from stdin and stores it in variable `x`, returning value
    - `(read T) -> U` is defined for `T: string | (), U: string`, e.g. you can use `(read ())` to avoid providing a var

### Control Flow
- `?` or `if`
    - `(if true "option1" "option2") = (? false "option2" "option1") = "option1`
    - `(? T U U) -> U` is defined for `T: bool, U: any`
- `while`
    - `(while true (print "ABC"))` prints "ABC" forever
    - `(while T U) -> ()` is defined for `T: bool, U: any`

### Boolean Operations
- `==` or `eq`
    - `(== 1 1) = (eq 'a' 'a') = true`
    - `(== T T) -> bool` defined for all default types
- `<>` or `neq`
- `<=` or `leq`
- `>=` or `geq`
- `<` or `lt`
- `>` or `gt`
- `&&` or `and`
- `||` or `or`

### Collection methods
- `++` or `concat` 
    - `(++ [1, 2, 3] [4, 5, 6]) = (concat [1] [2, 3, 4, 5, 6]) = [1, 2, 3, 4, 5, 6]`
    - `(++ list<T> list<T>) -> list<T>` defined for `T: any` (which includes strings)
- `:` or `prepend`
    - `(: 1 [2, 3, 4]) = (prepend 1 [2, 3, 4]) = [1, 2, 3, 4]`
    - `(: T list<T>) -> list<T>` defined for `T: any` (which includes strings, where `T: char`)
- `take`
    - `(take 2 [1, 2, 3, 4, 5]) = [1, 2]`
    - `(take T list<U>) -> list<U>` is defined for `T: uint, U: any`
- `split`
    - `(split 3 [1, 2, 3, 4, 5]) = ([1, 2, 3], [4, 5])`
    - `(split T list<U>) -> (list<U>, list<U>)` is defined for `T: uint, U: any`

### Variable management
- `def`
    - `(def x 12)` defines a new variable `x` with value `12` and type `int`
    - `(def <ident> T) -> ()` is defined for `T: any`
    - If you use a numeric literal the type will be set on the variable's definition. E.g. if you define `x` via 
    `(def x 12)` and then create a list like `[1u, x]` it will throw a compiler error. This may change in the future.
    You can get around this using the numeric prefix literals.
- `init`
    - `(init x int)` defines a new uninitialized variable with type `int`
    - `(init <ident> <type>) -> ()` is defined for any type and available identifier
- `set`
    - `(set x 12)` sets the value of existing variable `x` to 12 (coercing if necessary)
    - `(set x T) -> ()` is defined for `T = type(x)` (more or less)

### Convenience
- `tostring` converts any type to a default string representation
- `eval` evaluates an expression and throws the value away (specifically returns `()`)
    - Useful for putting multiple expressions in sequence, e.g. a program could look like
    ```
    ([
        (def x 12u)
        (set x (+ x 1))
        (eval (write (tostring x)))
    ])
    ```
    Without `eval` this would throw a type error trying to match `()` with `string`

## Grammar
My previous grammar was too low-level to be useful in the new implementation. Now that the `Token` type is fairly
well-defined I can do another quick pass. `<>` represent rules, `[]` represents tokens
```
<Prog> => <Expr>

<Expr> => [LParen] <ExprBody> [RParen]
<ExprBody> => <Val> | <FuncCall>

<Val> => <List> | <Expr> | [Ident] | [Type] | [CharLiteral] | [String] | [NumLiteral] | [UnitLiteral]

<List> => [LBrack] <ListBody> [RBrack]
<ListBody> => <Val> | <Val> <ListBody>

<FuncCall> => [ReservedIdent] <Args>
<Args> => <Val> | <Val> <Args>
```

Below I list which tokens can begin which rules:
```
<Prog>: [LParen]

<Expr>: [LParen]
<ExprBody>: [LBrack], [LParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral], [ReservedIdent]

<Val>: [LBrack], [LParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]

<ListVal>: [LBrack]
<ListBody>: [LBrack], [LParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral], [ReservedIdent]

<FuncCall>: [ReservedIdent]
<Args>: [LBrack], [LParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]
```

And now the tokens that can end each rule:
```
<Prog>: [RParen]

<Expr>: [RParen]
<ExprBody>: [RBrack], [RParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]

<Val>: [RBrack], [RParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]

<ListVal>: [RBrack]
<ListBody>: [RBrack], [RParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]

<FuncCall>: [RBrack], [RParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]
<Args>: [RBrack], [RParen], [Ident], [Type], [CharLiteral], [String], [NumLiteral], [UnitLiteral]
```

### Token Following
I need to know which rules each token can be a part of, and which other tokens can follow them.

#### LParen
`LParen` can only exist at the beginning of an expression, and it *always* exists at the beginning of any expression.
When we encounter an `LParen` it starts the `Expr` rule. This means we have the following options for next token:
```
[LBrack] =>        Means we are in Expr -> ExprBody -> Val -> ListVal rule now
[LParen] =>        Means we are in a new Expr
[Ident] =>         *TERMINAL* means we have resolved ExprBody = [Ident] and can move on
[Type] =>          *TERMINAL* means ExprBody = [Type] 
[CharLiteral] =>   *TERMINAL* means ExprBody = [CharLiteral]
[String] =>        *TERMINAL* means ExprBody = [String] 
[NumLiteral] =>    *TERMINAL* means ExprBody = [NumLiteral]
[UnitLiteral] =>   *TERMINAL* means ExprBody = [UnitLiteral]
[ReservedIdent] => Means we are in Expr -> FuncCall rule now
```

## Parser Edge Cases
- With my current handling of number literals something like `12u13` would be considered two separate numeric literals
`12u` and `13`
