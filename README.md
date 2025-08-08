LISP C Environment
===

Introduction
---

The Lisp C Environment is just another macro language for converting C to LISP based on concepts from [eratosthenesia/lispc](https://github.com/eratosthenesia/lispc).
It runs in a standard Lisp REPL to retain the maximum Lispy-ness of the language. 
It changes Lisp input such as this:
```lisp
(header "stdio.h")

(defun main int ((argc int) (*argv[] char))
  (printf "Hello, world!\\n")
  (return 0))
```
into this:
```c
#include <stdio.h>
int main(int argc, char *argv[])
{
  printf("Hello, world!\n");
  return 0;
}
```

Why
---

Lisp has macros. **Proper** macros. However, Lisp requires a virtual machine to run.
C is **natively compiled** and **fast**. But C **doesn't** have macros.

Usage
---

This Lisp library is a **Work In Progress** with functionality being added as I encounter it.
Not surprisingly, documentation will be lacking for a while for this project.
If you have any additions or requests, you can make a pull request or raise an issue.

Writing Lisp
---

The Lisp C Environment runs in a case-sensitive lisp package `C-ENV`.
The side-effects of this is that any proper Lisp code has to be written in upper-case to interact with anything.
So writing a Lisp macro in the Lisp C Environment looks like:

```lisp
(DEFMACRO example-if-lisp-expr-macro (test if-stmt &OPTIONAL else-stmt)
  (IF test if-stmt else-stmt))
```

To extend the c language, I have been using macros (Which you can also use) to create bindings to lisp-like declarations.
A helper function `RESOLVE-EXPR` converts basic lisp expressions into c code.
`*standard-output*` has been remapped to the output file so `(FORMAT T data)` and `(WRITE-STRING data)` can be utilized to create c output.
For example:

```lisp
(DEFMACRO and (lhs rhs)
 `(PROGN
    (WRITE-STRING "(")
    (RESOLVE-EXPR lhs)
    (WRITE-STRING " && ")
    (RESOLVE-EXPR rhs)
    (WRITE-STRING ")")
    T)) ; Returning T signals to RESOLVE-EXPR that a complete form will require a ';' at the end
        ; Returning NIL signals that we never want a ';' to succeed the expression

;(and t nil) => (true && false)
```

Keywords
---

## lambda-list

```text
lambda-list::= ((var type-specifier)*  
                  [&optional (var type-specifier init-form)*]  
                  [&rest var]  
                  [&key ((var type-specifier [init-form]))*])  
```
*var*---a *symbol*  

*type-specifier*---a *symbol*  

*init-form*---any *form*. If var isn't supplied, init-form is inlined in the expression  

## defun

### Syntax:

defun *function-name-and-options lambda-list form\**  
=> *macro-definition*  

function-name-and-options::= function-name | (function-name [function-modifier])  

function-modifier::= :static | :extern | :auto | :register  

**Arguments and Values:**  

*function-name*---a *symbol*  

*lambda-list*---an *ordinary lambda-list*  

*forms*---an *implicit progn*  

## call

### Syntax:

call *function-name argument\**

## block

### Syntax:

block *block-name form\**

## return

### Syntax:

return *value*

## dotimes

### Syntax:

dotimes *(var value) form\**

## header

### Syntax:

header *path [path-type]*

## headers

### Syntax:

headers *{path | (path [path-type])}*

## aref

### Syntax:

aref *form index\**

## mul

### Syntax:

mul *expr\**

## div

### Syntax:

div *expr\**

## add

### Syntax:

add *expr\**

## sub

### Syntax:

sub *expr [expr]\**

## if

### Syntax:

if *test if-form [else-form]*

## cond

### Syntax:

cond *cond-body\**

cond-body::= (test form*)

## comment

### Syntax:

comment *string\*"

## defvar

### Syntax:

defvar *var-name type-specifier [[init-form] [modifier]]*

## let

### Syntax:

let *var-bindings form\**

var-bindings::= ((var-name type-specifier [[init-form] [modifier]])*)

## progn

### Syntax:

progn *form\**

## mod

### Syntax:

mod *expr expr*

## do

### Syntax:

do *test-form form\**

## while

### Syntax:

while *test-form form\**

## logical

### Syntax:

logical *operator left-form right-form*

## and

### Syntax:

and *left-form right-form*

## or

### Syntax:

or *left-form right-form*
