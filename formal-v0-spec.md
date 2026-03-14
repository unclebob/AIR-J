# AIR-J Formal Specification v0

This document defines the version 0 language contract for AIR-J. It is written to support parser, normalizer, type checker, effect checker, and JVM backend implementation.

If this document conflicts with a future ergonomic authoring layer, this document wins for persisted AIR-J artifacts.

## 1. Scope

Version 0 includes:
- module declarations
- optional host-backed module classes for callback-oriented JVM frameworks
- explicit imports and exports
- nominal records
- enums
- tagged unions
- top-level functions
- lambda expressions
- first-class design by contract declarations
- explicit types on persisted artifacts
- explicit effects
- a small closed expression language
- explicit Java interop
- direct lowering to JVM bytecode
- canonical primitive operators and text/sequence built-ins where they reduce interop churn for agents

Version 0 excludes:
- macros
- traits or typeclasses
- user-defined effect handlers
- reflection as a core language feature
- implicit type inference in persisted AIR-J
- hygienic syntax expansion
- source-level concurrency constructs beyond foreign interop

## 2. Conformance

An AIR-J implementation is conformant for v0 if it:
- accepts all well-formed v0 modules
- rejects ill-formed modules with structured diagnostics
- normalizes accepted modules to the canonical form defined here
- type-checks and effect-checks normalized modules according to this spec
- lowers accepted modules to JVM artifacts with behavior equivalent to AIR-J semantics

## 3. Lexical Model

AIR-J source is an s-expression language.

Lexical categories:
- `symbol`: identifier token used for local names, module names, field names, type names, and keywords
- `integer`: base-10 signed integer literal
- `string`: double-quoted UTF-8 string literal with escaped quotes and escaped control characters
- `char`: single quoted character literal
- `bool`: `true` or `false`
- `null`: not a standalone literal in v0 source; null is represented only through foreign interop and typed as `Nullable[T]`

Whitespace and comments are not semantically significant.

Reserved head symbols:
- `module`
- `host`
- `imports`
- `export`
- `data`
- `enum`
- `union`
- `variant`
- `field`
- `type-params`
- `fn`
- `lambda`
- `params`
- `returns`
- `effects`
- `requires`
- `ensures`
- `invariants`
- `let`
- `if`
- `match`
- `case`
- `call`
- `construct`
- `record-get`
- `loop`
- `recur`
- `try`
- `catch`
- `finally`
- `raise`
- `seq`
- `var`
- `set`
- `local`
- `java/type`
- `java/new`
- `java/call`
- `java/static-call`
- `java/get-field`
- `java/set-field`

Reserved head symbols must not be used as local binders.

## 4. Canonical Representation

Persisted v0 AIR-J must be canonical.

Canonicality requirements:
- every node kind has exactly one persisted shape
- host-backed modules use exactly one persisted host clause shape
- all names are fully resolved to their canonical spelling
- all effect lists are sorted lexicographically
- imports are sorted lexicographically by module/class and then symbol
- exports are sorted lexicographically
- top-level declarations appear in this order:
  1. `imports`
  2. `export`
  3. zero or more type declarations
  4. zero or more functions
- all local references use `(local name)`; bare symbols are not a canonical expression form
- all literals appear directly as literal leaves, not wrapped in `(literal ...)`
- all syntactic sugar is removed before persistence

The persisted form is therefore a normalized AST rendered as s-expressions.

## 5. Grammar

The grammar below is normative for canonical v0 AIR-J.

```ebnf
module            ::= "(" "module" module-name host? imports export decl* ")"
host              ::= "(" "host" java-class-name ")"
imports           ::= "(" "imports" import* ")"
import            ::= airj-import | java-import
airj-import       ::= "(" "airj" module-name symbol+ ")"
java-import       ::= "(" "java" java-class-name ")"
export            ::= "(" "export" symbol* ")"

decl              ::= data-decl | enum-decl | union-decl | fn-decl

data-decl         ::= "(" "data" type-name type-params? invariants? field+ ")"
enum-decl         ::= "(" "enum" type-name variant-name+ ")"
union-decl        ::= "(" "union" type-name type-params? invariants? union-variant+ ")"
union-variant     ::= "(" "variant" variant-name field* ")"
field             ::= "(" "field" field-name type-expr ")"
type-params       ::= "(" "type-params" type-var+ ")"
invariants        ::= "(" "invariants" contract-expr+ ")"

fn-decl           ::= "(" "fn" fn-name params returns effects requires? ensures? expr ")"
params            ::= "(" "params" param* ")"
param             ::= "(" local-name type-expr ")"
returns           ::= "(" "returns" type-expr ")"
effects           ::= "(" "effects" effect-name* ")"
requires          ::= "(" "requires" contract-expr+ ")"
ensures           ::= "(" "ensures" contract-expr+ ")"

expr              ::= literal
                    | local-expr
                    | let-expr
                    | if-expr
                    | match-expr
                    | call-expr
                    | lambda-expr
                    | construct-expr
                    | variant-expr
                    | record-get-expr
                    | loop-expr
                    | recur-expr
                    | try-expr
                    | raise-expr
                    | seq-expr
                    | var-expr
                    | set-expr
                    | java-new-expr
                    | java-call-expr
                    | java-static-call-expr
                    | java-get-field-expr
                    | java-set-field-expr

local-expr        ::= "(" "local" local-name ")"
let-expr          ::= "(" "let" "(" binding* ")" expr ")"
binding           ::= "(" local-name expr ")"
if-expr           ::= "(" "if" expr expr expr ")"
match-expr        ::= "(" "match" expr case+ ")"
case              ::= "(" "case" pattern expr ")"
call-expr         ::= "(" "call" callable-expr expr* ")"
lambda-expr       ::= "(" "lambda" params returns effects expr ")"
construct-expr    ::= "(" "construct" type-name expr* ")"
variant-expr      ::= "(" "variant" type-name variant-name expr* ")"
record-get-expr   ::= "(" "record-get" expr field-name ")"
loop-expr         ::= "(" "loop" "(" loop-binding* ")" expr ")"
loop-binding      ::= "(" local-name expr ")"
recur-expr        ::= "(" "recur" expr* ")"
try-expr          ::= "(" "try" expr catch* finally? ")"
catch             ::= "(" "catch" type-expr local-name expr ")"
finally           ::= "(" "finally" expr ")"
raise-expr        ::= "(" "raise" expr ")"
seq-expr          ::= "(" "seq" expr expr+ ")"
var-expr          ::= "(" "var" local-name type-expr expr ")"
set-expr          ::= "(" "set" local-name expr ")"

java-new-expr         ::= "(" "java/new" java-class-name type-args? expr* ")"
java-call-expr        ::= "(" "java/call" expr member-id signature expr* ")"
java-static-call-expr ::= "(" "java/static-call" java-class-name member-id signature expr* ")"
java-get-field-expr   ::= "(" "java/get-field" expr field-name type-expr ")"
java-set-field-expr   ::= "(" "java/set-field" expr field-name type-expr expr ")"
type-args             ::= "(" "type-args" type-expr* ")"
signature             ::= "(" "signature" "(" type-expr* ")" type-expr ")"
contract-expr         ::= expr

callable-expr      ::= local-expr | lambda-expr

pattern            ::= wildcard-pattern
                    | literal-pattern
                    | binder-pattern
                    | enum-pattern
                    | union-pattern
                    | record-pattern
wildcard-pattern   ::= "_"
literal-pattern    ::= literal
binder-pattern     ::= local-name
enum-pattern       ::= variant-name
union-pattern      ::= "(" variant-name pattern* ")"
record-pattern     ::= "(" "record" type-name record-field-pattern+ ")"
record-field-pattern ::= "(" field-name pattern ")"

type-expr          ::= primitive-type
                    | type-name
                    | type-var
                    | tuple-type
                    | array-type
                    | list-type
                    | map-type
                    | nullable-type
                    | result-type
                    | fn-type
                    | java-type

tuple-type         ::= "(" "Tuple" type-expr type-expr+ ")"
array-type         ::= "(" "Array" type-expr ")"
list-type          ::= "(" "List" type-expr ")"
map-type           ::= "(" "Map" type-expr type-expr ")"
nullable-type      ::= "(" "Nullable" type-expr ")"
result-type        ::= "(" "Result" type-expr type-expr ")"
fn-type            ::= "(" "Fn" "(" type-expr* ")" type-expr effects ")"
java-type          ::= "(" "Java" java-class-name ")"
```

## 6. AST Schema

The compiler-facing AST must preserve the following node information.

### 6.1 Module

```text
Module {
  name: ModuleName
  imports: Import[]
  exports: Symbol[]
  decls: Decl[]
}
```

### 6.2 Imports

```text
Import =
  | AirjImport { module: ModuleName, symbols: Symbol[] }
  | JavaImport { className: JavaClassName }
```

### 6.3 Declarations

```text
Decl =
  | DataDecl {
      name: TypeName
      typeParams: TypeVar[]
      invariants: ContractExpr[]
      fields: FieldDecl[]
    }
  | EnumDecl {
      name: TypeName
      variants: VariantName[]
    }
  | UnionDecl {
      name: TypeName
      typeParams: TypeVar[]
      invariants: ContractExpr[]
      variants: UnionVariantDecl[]
    }
  | FnDecl {
      name: Symbol
      params: Param[]
      returnType: Type
      effects: Effect[]
      requires: ContractExpr[]
      ensures: ContractExpr[]
      body: Expr
    }
```

### 6.4 Expressions

```text
Expr =
  | BoolLit
  | IntLit
  | StringLit
  | CharLit
  | Local { name: Symbol }
  | Let { bindings: Binding[], body: Expr }
  | If { test: Expr, thenBranch: Expr, elseBranch: Expr }
  | Match { target: Expr, cases: Case[] }
  | Call { callee: Expr, args: Expr[] }
  | Lambda { params: Param[], returnType: Type, effects: Effect[], body: Expr }
  | Construct { typeName: TypeName, args: Expr[] }
  | Variant { typeName: TypeName, variantName: VariantName, args: Expr[] }
  | RecordGet { target: Expr, fieldName: Symbol }
  | Loop { bindings: Binding[], body: Expr }
  | Recur { args: Expr[] }
  | Try { body: Expr, catches: CatchClause[], finallyExpr?: Expr }
  | Raise { expr: Expr }
  | Seq { exprs: Expr[] }
  | Var { name: Symbol, varType: Type, init: Expr }
  | Set { name: Symbol, expr: Expr }
  | JavaNew { className: JavaClassName, typeArgs: Type[], args: Expr[] }
  | JavaCall { target: Expr, memberId: Symbol, signature: Signature, args: Expr[] }
  | JavaStaticCall { className: JavaClassName, memberId: Symbol, signature: Signature, args: Expr[] }
  | JavaGetField { target: Expr, fieldName: Symbol, fieldType: Type }
  | JavaSetField { target: Expr, fieldName: Symbol, fieldType: Type, expr: Expr }
```

### 6.5 Patterns

```text
Pattern =
  | Wildcard
  | LiteralPattern { literal: Literal }
  | BinderPattern { name: Symbol }
  | EnumPattern { variantName: VariantName }
  | UnionPattern { variantName: VariantName, args: Pattern[] }
  | RecordPattern { typeName: TypeName, fields: RecordFieldPattern[] }
```

### 6.6 Types

```text
Type =
  | PrimitiveType
  | NamedType { name: TypeName, args: Type[] }
  | TypeVar { name: Symbol }
  | TupleType { members: Type[] }
  | ArrayType { element: Type }
  | ListType { element: Type }
  | MapType { key: Type, value: Type }
  | NullableType { inner: Type }
  | ResultType { ok: Type, err: Type }
  | FnType { params: Type[], returnType: Type, effects: Effect[] }
  | JavaType { className: JavaClassName }
```

## 7. Well-Formedness Rules

Before typing, a module must satisfy all of the following.

### 7.1 Module Rules

- exactly one module declaration per file
- module name matches the file’s intended logical identity
- import entries are unique
- export names are unique
- every exported symbol is declared in the module
- top-level declaration names are unique within their namespaces

### 7.2 Declaration Rules

- record field names are unique within a record
- enum variant names are unique within an enum
- union variant names are unique within a union
- union field names are unique within a variant
- type parameters are unique within a declaration
- function parameter names are unique
- contract expressions on declarations resolve only names in the declaration contract scope

### 7.3 Expression Rules

- every local reference resolves to an in-scope binding
- every `set` target resolves to a mutable binding introduced by `var`
- every `recur` appears only in the tail position of a `loop` body
- every `catch` binder is unique within its clause
- `seq` contains at least two expressions
- `if` test, `match` target, and all call operands are valid expressions

## 8. Normalization Rules

Normalization is required before type checking.

The normalizer must:
- replace all bare identifier expression references with `(local ...)`
- sort imports and exports
- sort effect lists
- preserve declaration section order as `type-params`, `invariants`, `fields` for data/union and `params`, `returns`, `effects`, `requires`, `ensures`, `body` for functions
- reject unsupported sugar rather than preserve it
- resolve imported symbols to canonical names
- assign unique internal ids to local binders
- rewrite short-circuit sugar to `if` if an upstream layer introduced it
- rewrite host-specific integer widths to canonical AIR-J numeric literals
- reject implicit nulls

Normalization must not:
- invent effects not implied by the source
- change declared types
- reorder expressions where evaluation order matters

## 9. Type System

Typing judgments are of the form:

`Gamma; Delta |- expr : T ! E`

Where:
- `Gamma` is the immutable local environment
- `Delta` is the mutable binding environment
- `T` is the expression type
- `E` is the expression effect set

Contract judgments are of the form:

`Gamma; Delta |-c expr`

Meaning `expr` is a well-typed contract predicate under the contract environment.

### 9.1 Type Equality

Type equality is nominal for records, enums, unions, and named types.

Type equality is structural for:
- tuples
- arrays
- lists
- maps
- nullable
- result
- function types
- java types by fully qualified class name

Function type equality includes effect equality.

### 9.2 Subtyping

Version 0 defines no general subtyping relation.

Allowed compatibility conversions:
- exact type equality
- value of type `T` may be used where `Nullable[T]` is expected by implicit nullable injection

No other implicit conversions are allowed.

Numeric widening must be explicit via standard library or compiler-known conversion functions.

### 9.3 Literal Typing

- integer literal -> `Int` unless explicitly widened by surrounding canonical conversion
- string literal -> `String`
- char literal -> `Char`
- `true` and `false` -> `Bool`

### 9.4 Core Typing Rules

Contracts:
- every contract expression must type-check as `Bool`
- contract expressions must have inferred effect set `{}`
- contract expressions may reference only names made available by their declaration contract scope
- contracts are part of static program semantics, even if runtime checking is disabled

Local:
- if `x : T` in `Gamma` or `Delta`, then `(local x) : T ! {}`

Let:
- each binding is evaluated left-to-right
- binding effects accumulate
- later bindings may reference earlier bindings
- body is typed in the extended immutable environment

If:
- test must have type `Bool`
- both branches must have equal type `T`
- result type is `T`
- result effects are `effects(test) U effects(then) U effects(else)`

Match:
- target is typed first
- each case is typed in an environment extended by its pattern binders
- all case bodies must have equal type `T`
- result effects are the union of target and all case effects

Call:
- callee must have type `(Fn (T1 ... Tn) R (effects Ecallee))`
- arguments must type-check against parameter types exactly
- call result type is `R`
- call effects are `effects(callee) U effects(args) U Ecallee`

Lambda:
- body type must equal declared return type
- inferred body effect set must be a subset of declared effects
- lambda expression type is `(Fn (param-types) return-type (effects declared-effects))`
- creating a lambda does not execute its body and does not incur body effects

Construct:
- target type must be a declared record type
- arity must match field count
- each argument must match the corresponding field type

Variant:
- target type must be a declared union type
- variant must exist
- argument arity and types must match variant payload fields

Record-get:
- target must be a record type
- field must exist
- result type is the declared field type

Loop/Recur:
- loop bindings define the loop parameter types
- `recur` arity must match loop binder count
- every recur argument type must equal the corresponding loop binder type
- the loop body type is the loop result type

Try:
- body and every catch handler must have equal type `T`
- finally expression, if present, must type-check and its value is discarded
- result type is `T`

Raise:
- expression may have any type `E`
- `raise` may type-check against any expected result type
- `raise` contributes the `Foreign.Throw` effect unless raised value is modeled by a distinct implementation-defined effect

Seq:
- expressions evaluate left-to-right
- result type is the last expression type
- result effects are the union of all member effects

Var:
- initializer type must equal declared variable type
- expression result type is the declared variable type
- expression contributes `State.Write`
- mutable binding is added to `Delta`

Set:
- target must exist in `Delta`
- assigned expression type must equal the variable type
- result type is `Unit`
- expression contributes `State.Write`

### 9.5 Contract Scopes

Record and union invariants are checked in a contract scope containing:
- `self`, bound to the declared type being constrained

Function `requires` clauses are checked in a contract scope containing:
- all function parameters

Function `ensures` clauses are checked in a contract scope containing:
- all function parameters
- `result`, bound to the declared return type

Contract scopes do not permit:
- mutable bindings
- `recur`
- `raise`
- foreign interop forms

### 9.6 Contract Semantics

For a data or union declaration:
- every invariant must hold for every value constructed through well-typed AIR-J code
- the compiler may check invariants statically, dynamically, or both
- invariant violation is a program error

For a function declaration:
- every `requires` clause must hold at function entry
- every `ensures` clause must hold at normal function exit
- a call that cannot establish the callee's preconditions is invalid
- a function body that cannot establish its postconditions is invalid

Version 0 does not define blame assignment beyond structured diagnostics.

### 9.7 Pattern Typing

Wildcard:
- matches any target type
- binds nothing

Literal pattern:
- literal type must equal target type

Binder pattern:
- binds the target type to the binder

Enum pattern:
- target type must be the enum declaring the variant

Union pattern:
- target type must be the union declaring the variant
- nested pattern arity must match payload arity

Record pattern:
- target type must be the named record type
- every matched field must exist
- nested field patterns type-check against the corresponding field types

### 9.8 Exhaustiveness

The compiler must perform exhaustiveness checking for:
- enums
- unions
- booleans

The compiler may perform partial checking for records and literals.

Non-exhaustive `match` is a compile error in v0.

## 10. Effect System

Effects are modeled as symbolic capabilities.

An effect set is a finite set of effect names.

Version 0 standard effect names:
- `State.Read`
- `State.Write`
- `Files.Read`
- `Files.Write`
- `Network`
- `Clock.Read`
- `Random`
- `Process`
- `Foreign.Throw`
- `Concurrency`

### 10.1 Effect Judgment

Every expression typing rule computes an effect set.

For a function declaration:
- inferred body effects must be a subset of declared effects
- undeclared effects are a compile error

For a lambda:
- inferred body effects must be a subset of declared effects

Contract expressions must infer the empty effect set.

### 10.2 Effect Introduction

The following expressions introduce effects directly:
- `var` -> `State.Write`
- `set` -> `State.Write`
- `raise` -> `Foreign.Throw`
- `java/new` -> implementation-defined subset, at minimum declared foreign constructor effects
- `java/call` -> implementation-defined subset from resolved member metadata
- `java/static-call` -> implementation-defined subset from resolved member metadata
- `java/get-field` -> no intrinsic effect unless field access is modeled as foreign read by implementation policy
- `java/set-field` -> `State.Write` and any foreign write effects required by policy

### 10.3 Effect Propagation

Sequential composition, conditionals, pattern matching, and function calls propagate effects by set union.

Version 0 defines no effect polymorphism and no effect masking.

## 11. Design by Contract

Design by contract is a core language feature in v0.

### 11.1 Contract Forms

Allowed declaration-level contract forms:
- `(invariants expr+)` on `data` and `union`
- `(requires expr+)` on `fn`
- `(ensures expr+)` on `fn`

Contracts are interpreted conjunctively:
- all listed invariant expressions must hold
- all listed preconditions must hold
- all listed postconditions must hold

### 11.2 Purity Restriction

Contract expressions must be pure.

The following are forbidden inside contracts:
- `var`
- `set`
- `loop`
- `recur`
- `raise`
- `try`
- `java/new`
- `java/call`
- `java/static-call`
- `java/get-field`
- `java/set-field`

The intent is to keep contracts machine-checkable and side-effect free.

### 11.3 Runtime Checking

A conforming implementation may:
- erase contracts after static checking
- emit runtime checks for contracts
- emit both runtime checks and static diagnostics

If runtime checks are emitted:
- `requires` checks occur at function entry
- `ensures` checks occur before normal return
- invariant checks occur after construction and at implementation-defined safe points

### 11.4 Canonical Contract Representation

In canonical v0 AIR-J, contracts are not represented through `meta`.

The following forms are canonical:
- `data` and `union` declarations use `(invariants ...)`
- `fn` declarations use `(requires ...)` and `(ensures ...)`

Non-semantic metadata may still exist in future versions, but it is not part of the v0 core language.

## 12. Java Interop

Java interop nodes are legal only when the referenced class/member resolves during compilation.

### 12.1 Resolution Requirements

- `java/new` class must exist
- `java/call` target receiver type must be compatible with the resolved declaring class
- `java/static-call` class and method must exist and be static
- `java/get-field` and `java/set-field` field must exist
- provided signature must uniquely identify the intended member

### 12.2 Java Type Mapping

AIR-J to JVM primitive mapping:
- `Bool` -> `boolean`
- `Byte` -> `byte`
- `Short` -> `short`
- `Int` -> `int`
- `Long` -> `long`
- `Float` -> `float`
- `Double` -> `double`
- `Char` -> `char`
- `Unit` -> `void` where allowed, otherwise runtime unit encoding

AIR-J reference mapping:
- `String` -> `java.lang.String`
- `(Java C)` -> JVM reference type `C`
- `(Java "[...")` -> JVM array descriptor `[...]`
- `(Nullable T)` -> reference or boxed representation as required by lowering

Version 0 does not permit `(Nullable Int)` to lower to raw JVM `int`; it must lower to a boxed or wrapped representation.

### 12.3 Null Handling

Java-originated `null` is legal only at type `(Nullable T)`.

Using a value of type `(Nullable T)` where `T` is required is a type error unless transformed by an explicit null-handling function in library space.

## 13. Declaration Semantics

### 13.1 Records

Records are nominal and closed.

For a record declaration:
- field order is semantically significant for construction and lowering
- field names are part of the API contract
- structural equivalence with another record does not imply type equality
- invariants are part of the semantic contract of the type

### 13.2 Enums

Enums are nominal, closed, and payload-free.

Enum variants are singleton values.

### 13.3 Unions

Unions are nominal, closed sum types with variant-specific payload layouts.

Variant order is semantically stable for lowering metadata but not for source-level equality.

Union invariants are part of the semantic contract of the type.

## 14. Operational Semantics

Version 0 uses strict left-to-right evaluation.

Rules:
- function call arguments evaluate left-to-right
- `let` bindings evaluate left-to-right
- `seq` evaluates left-to-right and returns the last value
- `if` evaluates only the chosen branch
- `match` evaluates the target once, then the selected case body
- `try` evaluates `finally` after body or catch, if present
- `raise` aborts the current control path until a matching `catch` is found

## 15. JVM Lowering Contract

The backend must preserve AIR-J semantics under the following contract.

### 15.1 Modules

A module lowers to one or more `.class` files.

At minimum:
- one primary class per module
- additional synthetic classes for lambdas, unions, or helpers as needed

Module names map to JVM internal names by a deterministic implementation-defined encoding of `/`.

### 15.2 Functions

Top-level pure or impure functions lower to JVM methods.

Requirements:
- function name to method name mapping is deterministic
- parameter order is preserved
- return type mapping follows AIR-J to JVM type mapping
- declared effects do not require runtime representation unless emitted as metadata or contract checks

For the symbol `main`, the backend may reserve the JVM method name `main` for the launcher entrypoint wrapper.

If it does so:
- the AIR-J function named `main` must still lower deterministically to a non-conflicting JVM method name
- same-module AIR-J calls must target the lowered AIR-J method name, not the JVM wrapper
- the wrapper mapping must remain stable within a build target

### 15.3 Lambdas

Lambdas may lower as:
- synthetic classes implementing a function interface
- `invokedynamic` bootstrap-based lambdas

The chosen strategy must preserve captured variable values and AIR-J type semantics.

### 15.4 Records

Records lower to final classes with:
- one field per declared AIR-J field
- a constructor matching declared field order
- field accessors or direct field layout as implementation chooses
- optional invariant-check support if contract runtime checking is enabled

### 15.5 Enums

Enums lower either to:
- JVM enums, or
- final classes with singleton instances

The choice must remain consistent within a compiler build target.

### 15.6 Unions

Version 0 recommends lowering unions to one abstract base plus one final subclass per variant.

Alternative lowering to a shared tagged-object runtime is permitted only if:
- variant identity remains preserved
- payload field typing remains preserved
- pattern match lowering remains equivalent

### 15.7 Contracts in Lowering

If runtime contract checking is enabled, the backend must preserve source-level contract behavior under the chosen lowering strategy.

Permitted strategies include:
- inline checks in generated methods and constructors
- synthesized helper methods
- sidecar verifier artifacts

### 15.8 Program Entrypoints

Version 0 defines an optional JVM launcher entrypoint for exported AIR-J `main`.

A module is launcher-runnable if:
- `main` appears in the module export list
- there is exactly one top-level AIR-J function named `main`
- its parameter list is either empty or exactly one parameter of type `(Java "[Ljava.lang.String;")`

For a launcher-runnable module, the backend must emit:
- a public static JVM method `main(String[])`

Wrapper behavior:
- if AIR-J `main` has no parameters, the wrapper ignores JVM arguments
- if AIR-J `main` has parameter type `(Java "[Ljava.lang.String;")`, the wrapper passes the received `String[]` directly
- if AIR-J `main` returns `Unit`, the wrapper returns normally after evaluation
- if AIR-J `main` returns `Int`, the wrapper must use that value as the JVM process exit code
- for any other non-`Unit` return type, the wrapper may ignore the returned value after evaluation
- wrapper behavior must preserve AIR-J side effects and exception behavior

If a module does not satisfy launcher-runnable conditions, the compiler may omit the JVM wrapper.

Compiler-driver behavior is distinct from wrapper behavior:
- a source-level `run` command may invoke the lowered AIR-J entrypoint directly instead of invoking the emitted JVM wrapper
- if it does so, it may return the AIR-J `main` result to the host process rather than converting `Int` to a process exit

## 16. Diagnostics

A conforming implementation must emit structured diagnostics for parse, normalization, type, effect, and lowering errors.

Each diagnostic must include:
- machine-readable code
- severity
- source span if available
- primary message
- structured attached data where relevant, such as expected and actual types

## 17. Minimal Standard Library Assumptions

Version 0 assumes, but does not fully specify, a minimal library surface providing:
- boolean and numeric operators as ordinary functions
- explicit numeric conversions
- canonical `Option`, `Result`, and `Diagnostic` carriers in `airj/core`
- wrapper functions that convert common recoverable failures into `Result`
- a minimal filesystem boundary in `airj/file`
- canonical sequence operations over `(Seq T)` when sequence values are exposed
- canonical keyed structures over `(Map String T)` when named lookup is exposed
- line-oriented filesystem helpers only when they still return canonical AIR-J data such as `(Seq String)` and `Result`

These are library assumptions, not core syntax.

## 18. Example Canonical Module

```lisp
(module app/http
  (imports
    (airj core/ops add eq)
    (java java.time.Instant))
  (export
    Method
    Request
    Response
    route)
  (data Request
    (invariants
      true)
    (field path String)
    (field method Method))
  (enum Method Get Post)
  (data Response
    (field status Int)
    (field body String))
  (fn route
    (params (req Request))
    (returns Response)
    (effects ())
    (requires
      true)
    (ensures
      true)
    (match (record-get (local req) path)
      (case "/" (construct Response 200 "ok"))
      (case _ (construct Response 404 "not-found")))))
```

## 19. Implementation Order

The recommended implementation order for v0 is:
1. parser
2. AST data model
3. normalizer
4. name resolution
5. type checker
6. contract checker
7. effect checker
8. exhaustiveness checker
9. Java resolver
10. JVM lowering backend

This order is non-normative, but it matches the dependency structure of the spec.
## Standard Modules

- `airj/core` contains canonical machine-facing carriers such as `Diagnostic`, `Option`, `Result`, and `Interchange`.
- `airj/file` contains canonical filesystem boundary operations.
- `airj/json` contains the canonical JSON parse/emit boundary for interchange values.
- Recoverable standard-module failures should prefer `Result ... Diagnostic` over raw exception text.
