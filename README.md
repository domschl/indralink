## Indralink

Indralink is a minimal stack language that takes inspiration from Forth, while using a VM and supporting streamlined data types.

Any aspect of Indralink is provisional and subject to change.

Target is to use Indralink for embedded scripting (e.g. with Muwerk)

## Preliminary language description

Indralink is primarily a stack language: functions operate on values that are pushed on the stack:

```indralink
1 2 +
```

### Functions

gives `3`. Functions can be defined with `: <func-name> ... ;` syntax similar to Forth. Comments are
anything between `( ... )` and (in files only) from `\` up to line-end, as in Forth.

In repl, functions are on one line, in files (`load`, `save` e.b.), function can be formatted arbitrarily.

```indralink
: plus2 (INT n -- n+2) 2 + ;    \ The comment in brackets () uses Forth style: an INT argument n is expected on stack, a result n+2 is put on stack.
```
then
```indralink
1 plus2 print  \ put 1 on stack, call function plus2 and print the result that is on the stack.
```

gives `3`.

### Variables

Local and global variables can be used, global variables start with `$`.

```
1 >a   \ store 1 into local variable a. a is only valid during execution of the current context, e.g. this works:
1 >a a 1 + print
```

Prints `2`.

```
1 >a
a print
```

Does not work, in the second expression, a is no longer known. Use globals for that:

```
3 >$a
a print
$a print
```

Both work, the second line looks for a local var `a`, if there is none, a global var `$a` is searched and found.

### Data-types

Known data-types are INT, FLOAT, BOOL, STRING, INT_ARRAY, FLOAT_ARRAY, STRING_ARRAY, BOOL_ARRAY

```
3.14 >$a  \ defines global float
"Hello, world" >$s  \ global string
true >$b  \ global bool
33 >$i \ global int
```