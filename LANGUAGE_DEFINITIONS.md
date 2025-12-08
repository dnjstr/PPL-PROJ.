# Language Definitions for Paradigms Used in this Project

This document provides compact BNF-style syntax and informal semantics for the four programming paradigms used in the repository: Imperative (C), Object-Oriented (Java), Functional (Common Lisp), and Logic (Prolog). Each language section contains:

- A. Syntax — compact BNF up through control structures
- B. Semantics — informal explanations grouped into:
  1. Basic Syntax
  2. Control Structures
  3. Data Types
  4. Subprograms

Use these definitions as documentation for course reports, README additions, or language-overview references.

---

## Imperative (C)

**A. Syntax — BNF (compact, up to control structures)**

```
<program> ::= { <declaration> }
<declaration> ::= <var-decl> | <func-decl>
<var-decl> ::= <type> <identifier> [ '=' <expression> ] ';'
<func-decl> ::= <type> <identifier> '(' [ <param-list> ] ')' '{' { <statement> } '}'
<param-list> ::= <param> { ',' <param> }
<param> ::= <type> <identifier>
<statement> ::= <expr-stmt> | <compound-stmt> | <if-stmt> | <switch-stmt> | <while-stmt> | <for-stmt> | <return-stmt> | <decl-stmt>
<expr-stmt> ::= <expression> ';'
<compound-stmt> ::= '{' { <declaration> | <statement> } '}'
<if-stmt> ::= 'if' '(' <expression> ')' <statement> [ 'else' <statement> ]
<while-stmt> ::= 'while' '(' <expression> ')' <statement>
<for-stmt> ::= 'for' '(' [ <expression> | <declaration> ] ';' [ <expression> ] ';' [ <expression> ] ')' <statement>
<return-stmt> ::= 'return' [ <expression> ] ';'
<expression> ::= <assignment>
<assignment> ::= <unary> { '=' <assignment> } | <binary-expr>
<binary-expr> ::= <expression> <op> <expression>
<unary> ::= [ '-' | '!' | '&' | '*' ] <primary>
<primary> ::= <identifier> | <constant> | '(' <expression> ')'
<constant> ::= integer | floating | char | string | 'NULL'
```

**B. Informal Semantics**

1. Basic Syntax
- Programs are collections of declarations (global variables, function definitions).
- Variable declarations allocate storage (global or automatic). Identifiers denote storage locations (l-values) or values (r-values).
- Expressions are evaluated following C's precedence and associativity rules; side-effects and sequence points determine well-defined behavior.

2. Control Structures
- `if`/`else`: executes branch based on condition (zero = false). Only selected branch executes.
- `while`/`for`: iterative loops; `for` combines init, condition, and post expressions.
- `switch` uses `case` labels with fall-through unless `break` used.

3. Data Types
- Primitive: `int`, `char`, `float`, `double`, `_Bool`.
- Derived: pointers, arrays, functions; composite: `struct`, `union`, `enum`.
- Implicit conversions and promotions apply in expressions; pointer arithmetic follows element-size.

4. Subprograms
- Functions declared with return type and parameter list; parameters passed by value (pointers carry references).
- Scope: block (local), file/global; static storage vs. automatic.
- Functions can mutate global state and return values; order-of-evaluation caveats must be observed.

---

## Object-Oriented (Java)

**A. Syntax — BNF (compact, up to control structures within methods)**

```
<compilation-unit> ::= { <package-decl> } { <import-decl> } { <type-decl> }
<type-decl> ::= <class-decl> | <interface-decl>
<class-decl> ::= ['public'|'private'|'protected'] 'class' <Identifier> [ 'extends' <Identifier> ] [ 'implements' <id-list> ] '{' { <member-decl> } '}'
<member-decl> ::= <field-decl> | <method-decl> | <constructor-decl>
<field-decl> ::= [modifiers] <type> <identifier> [ '=' <expression> ] ';'
<method-decl> ::= [modifiers] <type> <identifier> '(' [ <param-list> ] ')' [ 'throws' <id-list> ] '{' { <statement> } '}'
<statement> ::= <block> | <if-stmt> | <while-stmt> | <for-stmt> | <enhanced-for> | <return-stmt> | <expr-stmt>
<if-stmt> ::= 'if' '(' <expression> ')' <statement> [ 'else' <statement> ]
<while-stmt> ::= 'while' '(' <expression> ')' <statement>
<for-stmt> ::= 'for' '(' [ <for-init> ] ';' [ <expression> ] ';' [ <expression> ] ')' <statement>
<enhanced-for> ::= 'for' '(' <type> <identifier> ':' <expression> ')' <statement>
<expr-stmt> ::= <expression> ';'
```

**B. Informal Semantics**

1. Basic Syntax
- Classes define object types; `new` allocates on the heap and returns a reference.
- Methods and fields belong to the class; `static` members belong to the class itself.
- Java is strongly typed at compile time; `null` dereference causes `NullPointerException`.

2. Control Structures
- `if`/`else`, `for`, `while` behave like imperative languages but with well-specified evaluation order and exception propagation.
- `enhanced for` iterates over arrays/Iterables using the iterator protocol.
- Exceptions (`throw`) alter normal control flow, `try/catch/finally` handle them.

3. Data Types
- Primitive (`int`, `long`, `float`, `double`, `char`, `boolean`) are value types.
- Reference types (classes, interfaces, arrays) are referenced by pointers; equality semantics: `==` compares references, `.equals(...)` compares logical equality.
- Generics provide compile-time polymorphism via type parameters (type erasure at runtime).

4. Subprograms
- Methods belong to classes; instance methods use dynamic dispatch (based on runtime type).
- Parameters are passed by value; for reference types the reference is copied (object itself is not copied).
- Constructors initialize objects and may call `super(...)`.

---

## Functional (Common Lisp representative)

**A. Syntax — BNF (s-expression style)**

```
<program> ::= { <form> }
<form> ::= <atom> | '(' <operator> { <form> } ')'
<defun> ::= (defun <name> (<params>) <docstring>? { <form> })
<let> ::= (let ((<var> <init>) ...) { <form> })
<if> ::= (if <test> <then> [<else>])
<cond> ::= (cond (<test1> <expr1> ...) ... (t <default-expr>))
<lambda> ::= (lambda (<params>) { <form> })
<application> ::= (<func> <arg1> <arg2> ...)
```

**B. Informal Semantics**

1. Basic Syntax
- Lisp uses s-expressions (code as data). Evaluation is applicative: evaluate operator (function) and arguments, unless the operator is a special form or macro.
- Symbols and values are distinct namespaces for functions and variables.

2. Control Structures
- `if`: evaluates test then either then- or else-expression.
- `cond`: multi-branch choice; first true clause executes.
- Loops: recursion is idiomatic; `loop`, `do` and iterative constructs exist; macros provide custom control forms.

3. Data Types
- Numbers, characters, strings, symbols, cons cells (lists), vectors, hash-tables, structures (`defstruct`).
- Dynamic typing; many types are mutable, but functional style prefers immutable/persistent patterns.

4. Subprograms
- Functions are first-class, can be returned or passed as arguments.
- Closures capture lexical environment (when lexical scope used).
- Macros operate on the program’s code representation before runtime and can implement new syntactic constructs.

---

## Logic (Prolog representative)

**A. Syntax — BNF (Horn-clause style)**

```
<program> ::= { <clause> }.
<clause> ::= <fact> '.' | <rule> '.' | <directive> '.'
<fact> ::= <predicate>
<rule> ::= <predicate> ':-' <goal-list>
<predicate> ::= <atom> '(' [ <term-list> ] ')'
<goal-list> ::= <goal> { ',' <goal> }
<term> ::= <atom> | <number> | <variable> | <compound-term>
<compound-term> ::= <functor> '(' [ <term-list> ] ')'
<variable> ::= identifier starting with uppercase or '_'
```

**B. Informal Semantics**

1. Basic Syntax
- Facts are unconditional assertions (tuples). Rules are implications: head is true if body goals are true.
- Programs form a database that is queried using goals: Prolog attempts to prove goals using unification and resolution.

2. Control Structures
- Prolog uses SLD resolution with depth-first left-to-right search and backtracking to find solutions.
- The cut `!` prunes choice points (procedural control) and `\+ Goal` is negation as failure.
- Order of clauses and goals affects search and performance.

3. Data Types
- Atoms (symbols), numbers, variables, lists (linked pairs), and compound terms. No static types; predicates enforce expected shapes.
- Predicates can be dynamic (assert/retract) to change the knowledge base at runtime.

4. Subprograms
- Predicates represent relations; calling a predicate attempts to find variable bindings that satisfy the relation.
- Side effects (I/O, assert/retract) are available but reduce declarative purity.

---

If you want this file adjusted (add concrete examples from the repository, expand any BNF to fuller grammars, or place the content into a `docs/` folder), tell me where and I will update it.
