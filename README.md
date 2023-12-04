## Indralink

Indralink is a minimal stack language that takes inspiration from Forth, while using a VM and supporting streamlined data types.

Any aspect of Indralink is provisional and subject to change.

Target is to use Indralink for embedded scripting (e.g. with Muwerk)

## Build

Requires `cmake`, `ninja` (`brew install cmake ninja`)

```bash
mkdir build
cd build
cmake -G Ninja ..
ninja
```

start with:

```bash
indralink
```

## Preliminary language description

Indralink is primarily a stack language: functions operate on values that are pushed on the stack:

```indralink
1 2 +
```

gives `3` on stack. `print` shows the last stack content:

```
1 2 + print
```

outputs `3`.

### Functions

Functions can be defined with `: <func-name> ... ;` syntax similar to Forth. Comments are
anything between `( ... )` and (in files only) from `\` up to line-end, as in Forth.

In repl, functions are on one line, in files (`load`, `save` e.b.), function can be formatted arbitrarily.

```indralink
: plus2 2 + ;
```

or, equivalent with comments:

```indralink
: plus2 (INT n -- n+2) 2 + ;    \ The comment in brackets () uses Forth style: stack in/out
```

then

```indralink
1 plus2 print  \ put 1 on stack, call function plus2 and print the result from stack.
```

gives `3`.

```
!plus2    \ delete plus2 defintion
```

Deletes the function.

### Variables

Local and global variables can be used, global variable names start with `$`.

Assignment works from stack by prefix `>` to the variable name.

Prefix `!` deletes a variable

```
1 >a   \ store 1 into local variable a. a is only valid during execution of the current context, e.g. this works:
1 >a a 1 + print
```

Prints `2`" 1 is stored into `a`, then `a` is put on stack, 1 is added 
and the result is print from stack.

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

```
!$a
```

Deletes global variable a.

### Data-types

Known data-types are INT, FLOAT, BOOL, STRING, INT_ARRAY, FLOAT_ARRAY, STRING_ARRAY, BOOL_ARRAY

```
3.14 >$a  \ defines global float
"Hello, world" >$s  \ global string
true >$b  \ global bool
33 >$i \ global int
```

Global vars can be listed with `listvars`.

#### Arrays

Arrays must be of a single type, containing BOOL, INT, FLOAT, or STRING. They cannot be nested.

```
[1 2 3] >$a
a print
```

generates a 3-element integer array. It could alternatively be generated by:

```
1 3 range  \ generate int array in steps of 1 from `1` to and including `3`. `3 1 range` create the reverse order.
```

```
[true false true] >$b \ bool array
"hello" "" split >$c \ Split takes to arguments, the string to split and a split token, if the split token is empty, the string is split into single chars, and the result is stored into global string array c$. `"1dum2dum4dum" "dum" split` generates `[ "1" "2" "4" ]`. 
```

```
[0 2 4 6] 2 index print
```

gives `4`, zero-based array index 2 has value 4.

```
[0 2 4 6] 2 remove print
```

gives `[0 2 6]`.

`[1 2]` erase gives empty INT array `[]`

Empty arrays can be generated with `[int], [float], [string], [bool]`

### Flow control

#### `if`, `else`, `endif`

`if` expects a BOOL on stack (or an INT, 0=false):

```
1 >a a 2 == if "a=2" else "a!=2" endif print
```

This puts 1 into local var `a`, then compares `a` and `2` for equality, the result (here `false`) is put on stack. `if` checks the stack, sees `false` executes the `else`-branch and puts string "a!=2" on stack. After `endif` `print` prints the last element on stack.

#### `for`, `break`, `next`

The `for` expression iterates over an array on the stack. `break` exists the loop and `next` starts the next iteration:

```
1 10 range for print " " print next
```

generates `1 2 3 4 5 6 7 8 9 10`. `1 10 range` generates an integer-array with 10 elements from 1 to 10. The for loop iterates over the array and prints the current element followed by a `" "`.

```
1 10 range for dup dup 2 % 0 == if print " " print else drop endif 7 == if break endif next
```

Prints `2 4 6`: An array of ints 1..10 is pushed to stack. The for loop enumerates all elements, the current element on stack is duplicated twice, it's now on the stack in 3 copies. The first copy is used for mod 2 calculation, if divisible by two, the second copy is printed otherwise discarded by drop. The third copy is used for a compare with 7, and on equal, the loop is broken.

Alternatively a local variable could be used:

```
1 10 range for >n n 2 % 0 == if n print " " print endif n 7 == if break endif next
```

Prints `2 4 6` too: In the for loop the current element is stored into variable `n` by `>n`. Then `n` is put on the stack to calculate `n 2 %`. `n` is then used further to print the divisble elements and to check for abort.

#### `while`, `break`, `loop`

`while` expects a boolean on the stack: on `true`, the loop content is executed. `break` exists the loop at any time.

```
5 >n true while n print n 1 - >n n 0 > loop
```

gives "54321". 5 is stored to var `n`, true is set for intial `while` condition, `n` is printed and decremented by one. The next condition for the while-loop is the compare `n 0 >`, the loop continues as long as n>0.

## Various built-ins

### stack and heap

- `ss` stack size is put on stack as INT
- `cs` clear stack
- `listvars` show all global variables
- `listfuncs` list all defined functions
- `"filename" save` Save all currently defined functions into file "filename"
- `"filename" load` Load functions from "filename"
- `dup` duplicate last stack entry
- `drop` remove last entry from stack
- `dup2` last two stack elements: `a b` becomes `a b a b` on stack
- `swap` swap last two stack elements `a b` becomes `b a`
- `eval` evaluate a string as code
- `print` or `.` print last element on stack
- `printstack` or `ps` print entire stack

### Arrays

- `range` gnerates an array of successive ints starting using the last two stack elements (INT) as inclusive borders. `3 1 range` generates `[3 2 1]`.
- `remove`. Arguments from stack: array and (zero-based) index to be removed. `[1 2 3] 1 remove` generates `[1 3]`
- `append`. Append an element (must be of same type) to an array. `["Hello" "my"] "world" append` gives `["Hello" "my" "world"]`. Note that appending happens only on stack.
- `update`. Updates the array content at a given index. `[false true] 1 false update` gives `[false false]`
- `index`. Read array element at index: `[4.1 5.1 6.1] 1 index` gets `5.1`
- `len`. Puts array length on stack as INT.
- `erase`. Removes all elements from array, leaving an empty array.
- `[int], [bool], [string], [float]`. Create empty arrays of given type.

### Type conversion

- `array`. Convert BOOL, INT, FLOAT, or STRING into a corresponding array of length 1.
- `int`. Convert to int
- `float`. Convert to float
- `bool`. Convert to float (String: 'true', false, numbers: only 0 or 0.0 is false)
- `string`. Convert to string

### String handling

- `len`. Put string length on stack as INT.
- `split`. A string is cut at a token. `"abc" "" split` expands each char into `["a" "b" "c"]`. `"1-x2-x3" "-x" split` generates `["1" "2" "3"]`
- `substring`. A substring at given pos and length is extracted. `"Hello, world" 3 4 substring` gives `"lo, "`.
- `sum`. Concatenates STRING_ARRAY components into single string. `["a" "b" "c"] sum` gives `"abc"`.
- `+`. Concatenate two strings from stack. `"a" "b" +` gives `"ab"`.
  
### Arithmetic

- Dual operators are: `+`, `-`, `*`, `/`, `%`
- Compare: `==`, `!=`, `>=`, `<=`, `<`, `>`
- Boolean: `and`, `or`
- `sum`: Add all array elements


