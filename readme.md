# Transparent Monads syntax and Monadic Flow Control interpretation

If you are here for the first time, please, read the [tms and mfc article] (or in the root of repo or artifact)
which explains ideas and aims, maps intuition to implementation, shows some examples and limitations of macros usage.

`sugar-tms` is the library of macros with built-in set of implicits that enables additional Scala syntax and 
code interpretation where:
- by `transparentMonads` macro: 
  each expression of the monadic type (or of the monadic stack type) may be used in any place where value of 
  its any inner type is expected. In other words, treat the value of the `Monad[T]` type as the value of type `T` 
  wherever type `T` is expected for any depth of inner `Monad[T]` types recursively 
- by `monadicFlowControl` macro (along with above `transparentMonads` feature):
  sequence of statements is treated as joined by flatMaps-map of detected Monadic Flow Type.
  In other words, the statements (possibly grouped to subsequences) of the passed block are executed not just unconditionally 
  one by one (as usual flow control) but in terms of Monadic Flow Control (being joined like in the for-comprehension header: 
  statement (or group) execution is controlled by the previous monad)  

The aim is the easier defined and more clear business code where required obvious monadic manipulations 
(*for-yield, flatMap, map*) are derived and built by macros. So, compile time implicits satisfy typechecker and designate 
access to inners dropping the top of types stack, macros - map input code to the for-comprehensions stack that 
extracts the inners of a monadic stack values.  

The practical difference between these two macros is that `monadicFlowControl` enables the use of the local stable vals
as tms-values while limiting position (actually: the scope of usage) of definitions of the other types (lazy vals, var,
types, classes, objects, defs) to the resulting `yield` (after the last stable val definition or Monadic Flow Type expression).

Code translation difference: 
- `transparentMonads` macro does pointwise replacement of tms-values and tms-expressions to ones inner values 
  obtained in the embracing `for`s stack. After replacements, the entire passed input code becomes the `yield` part of the built `for`s stack.
- `monadicFlowControl` macro, along with `transparentMonads` transformations, maps the init of passed block statements to one `for` 
  of the built stack that corresponds to Monadic Flow Type of the first statement (val or expression). 
  Only tailing statements (after the last stable val definition and last expression of the Monadic Flow Type) become 
  the `yield` part of the built `for`s stack.     

Thus, `transparentMonads` is good for expressions (even complex) or blocks with any definitions that are not used as (or in) 
the Transparent Monads, `monadicFlowControl` - for blocks of statements with stable val definitions that are used as (or in) 
the Transparent Monads of the passed code.

## Table of contents
- [Usage](#usage)
  - [Required dependencies](#required-dependencies)
  - [Macros call variants](#macros-call-variants)
  - [Examples](#examples)
  - [How to test](#how-to-test)
- [Shortly how it works](#shortly-how-it-works)
  - [Transparent Monads syntax by implicits](#transparent-monads-syntax-by-implicits)
  - [Transparent Monads macro](#transparent-monads-macro)
  - [Monadic Flow Control macro](#monadic-flow-control-macro)
  - [Flattening the result](#flattening-the-result)
- [Limitations and notes](#limitations-and-notes)
  - [Type variables (type classes) and tts implicits](#type-variables-type-classes-and-tts-implicits)
  - [Array collections overload precedence](#array-collections-overload-precedence)
  - [Call by name caution](#call-by-name-caution)
  - [Macro output type and nested tms-expressions](#macro-output-type-and-nested-tms-expressions)
  - [Evaluation order](#evaluation-order)
  - [Local definitions limitation](#local-definitions-limitation)
  - [Implicits scope workaround](#implicits-scope-workaround)
  - [2.11 lazy vals scope is not verified](#211-lazy-vals-scope-is-not-verified)
  - [IDEA plugin syntax checks](#idea-plugin-syntax-checks)
- [Tms Options](#tms-options)
  - [Options summary](#options-summary)
  - [Order of applying](#order-of-applying)
  - [Pre-evaluation](#pre-evaluation)
  - [Inner Fors Stacks to control evaluation Order](#inner-fors-stacks-to-control-evaluation-order)
  - [Predef compliance (any2stringadd)](#predef-compliance-any2stringadd)
  - [Helper options](#helper-options)
- [Contributing](#contributing)
- [Versions](#versions)


## Usage
Artifact currently is being built for Scala 2.11, 2.12 & 2.13. Since starting Scala 2.12.13 Predef implicits are changed, 
to acknowledge the changes the macros code defers from compiled for previous 2.12 versions. That is why there are 
two different artifacts for Scala 2.12. Incorrect usage of 2.12 artifact with wrong Scala version will be reported 
as an error at compile time.

### Required dependencies
To use the macros add the following to library dependency:
- for Scala 2.11, 2.12.1-2.12.12 & 2.13 use classic Scala dependencies:

      "ua.org.sands" %% "sugar-tms" % "0.2.4"
  
- for Scala 2.12 starting 2.12.13 use:

      "ua.org.sands" % "sugar-tms_2.12at13+" % "0.2.4"

The artifact only depends on the "scala-reflect" and both are used exclusively at compile time (or toolbox code compile 
time during the toolbox tests - tests runtime) and do not require runtime dependency for your project. So, to exclude 
"sugar-tms" and "scala-reflect" dependency from publishing with you project add "compile-internal, test-internal" 
configuration to the dependency:

    "ua.org.sands" %% "sugar-tms" % "0.2.4" % "compile-internal, test-internal"

Please, be careful with IDEA, and drop this configuration if you have any troubles. Plugin still works strange with one 
while importing the project (sometimes it skips such dependency at all, but after the first import works stably with it).   

To use dependency for any Scala version you may add the following to project's setting:

    libraryDependencies += (if ("""^2\.12\.(\d+).*""".r.findFirstMatchIn(scalaVersion.value).exists(_.group(1).toInt >= 13))
      "ua.org.sands" % "sugar-tms_2.12at13+"
    else
      "ua.org.sands" %% "sugar-tms"
    ) % "0.2.4" % "compile-internal, test-internal",

This sample of usage may be found in the root project of the code. Root is made for the purpose of testing.

### Compatibility and versioning
Project does not require the runtime dependency on its artefact. Everything is done at compile time.

While the project is in research & growing state, it will follow the full backward versions compatibility for all 0.x.x versions.

For all new versions above means:
- semantic behaviour of old macros will not be changed (for the same input code the code produced be the macro will not 
  change semantically: monadic manipulation order, order of side effects, etc.), but may to overcome past limitations 
  extending the functionality that was previously errors
- any defaults will not be changed
- new functionality will be realized by new macro or by new option of existing macro
- new option for existing macro will have the default disabled state to repeat the old behaviour without macro usage changes

Such strategy simplifies lots of things for users, requires only one primary branch for developing & support and lets
to focus on new ideas and implementations rather than maintaining old versions code. 
It allows different idea realizations to exist simultaneously.

So, upgrading to new version will not require additional efforts on the old usages, will bring new semantic functionality 
with new macros reflected in the `minor` version up and bugs fixes or new helper functionality reflected by the `patch` version up.  

### Macros call variants
Macros are represented by full and sort names with and without options variants:
Transparent Monads macro have the following signatures that call the same implementation:
```scala
def transparentMonads[O](tmsCode: Any): O
def tms[O](tmsCode: Any): O
def transparentMonadsFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O
def tmsFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O
```
Monadic Flow Control macro implementation is based on Transparent Monads and includes its full functionality while 
adding Monadic Flow Control interpretation of the passed block of statements:
```scala
def monadicFlowControl[O](tmsCode: Any): O
def mfc[O](tmsCode: Any): O
def monadicFlowControlFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O
def mfcFor[O](tmsLiteralOptions: String*)(tmsCode: Any): O
```
`O` is the result type defined by required Transparent Monads extractions of the input `tmsCode`.
`tmsCode` may represent any Scala expression or the block of statements containing Transparent Monads.

Output `O` type and passed `tmsCode` should match by Transparent Monads' extraction possibility.
Not every `O` type may correspond to passed `tmsCode` but some variants of `tmsCode` may have several valid matching `O`.
This mapping is defined by Transparent Monads order and types in `tmsCode`. 

Tms options may additionally be specified by `@tmsOptions` annotation to apply options for all macros enclosed by 
annotated target (class, object, method). Default and overriding options for all macro calls may be specified by 
environment variables and/or system properties. 

### Examples
Usage of both macros in the code requires the only import: 
```scala
import sands.sugar.tms.TransparentMonads._ 
```
that imports all required implicits, macros and optional `@tmsOptions` annotation.

For example, the function that summaries the passed `Try[Int]` list:
```scala
import sands.sugar.tms.TransparentMonads._

def sumTries(intTries: Try[Int]*): Try[Int] = intTries.foldLeft(Try(0))((t1, t2) => tms[Try[Int]](t1 + t2))

// to see the compile time debug info call tmsFor with the debug option passed:
def sumTries(intTries: Try[Int]*): Try[Int] = intTries.foldLeft(Try(0))((t1, t2) => tmsFor[Try[Int]]("Debug")(t1 + t2))
```
macro will replace the input `t1 + t2` expression buy the following `for`
```scala
{
  for {
    valueOfTry$macro$1 <- t1
    valueOfTry$macro$2 <- t2
  } yield {
    valueOfTry$macro$1.$plus(valueOfTry$macro$2)
  }
}
```
`monadicFlowControl` macro enables the usage of local stable vals as Transparent Monads values. The sample is:
```scala
import sands.sugar.tms.TransparentMonads._

// assuming we have
case class User(personalData: Option[PersonalData])
def getUser(userId: String): Future[User]

// then we may get the Future[String] status of user by
def getUserEmails(userId: String) =
  mfc[Future[String]] {
    val user = getUser(userId) // Future[User]
    if (user.personalData.isEmpty) // direct access to User properties on Future[User] value
      "should provide personal data"
    else
      "may operate"
  }

// macro generated block is
{
  for {
    userValue$macro$1 <- getUser(userId)
  } yield {
    if (userValue$macro$1.personalData.isEmpty)
      "should provide personal data"
    else
      "may operate"
  }
}
```

### How to test
To run internal tests you should import the project, select required Scala version by 
changing `build.sbt` (default is 2.13) or by sbt command `++2.12.12`, for example.

To run tests for selected Scala version execute sbt command `sugar-tms/test`.

Header comments of the `build.sbt` describes additions options to run the tests. For example, to run any test in IDEA with 
debug & trace output you may add system property `-DtmsTestDebug=true` to VM Options (by default `tmsTestDebug=false` and 
`tmsTestTrace=tmsTestDebug`) of the test run configuration (or scalatest configuration pattern for all test suites).
Or add environment variable `tmsTestDebug` to always see the macros debug output (for the sbt and IDEA runs).

To run tests for all supported scala versions (with the latest hardcoded patch versions) 
just execute sbt command `+sugar-tms/test`.

For Scala 2.13 project passes 4500 testcases (the part of tests run with different tms option combinations).

Project is compiled for target `jvm-1.8` and is tested with JDK8, JDK11 & JDK16. Project's root folder contains `tools` 
directory with batch files to run tests under different JDK versions.  

Sbt `test` command will only execute simple root project tests to test the public symbols accessibility of the artifact 
or source `sugar-tms` code: simple macros calls in the native and toolbox tests. 
Version of the tested artifact is hardcoded in `build.sbt`. When not specified then the source project will be used. 
For all Scala versions these root tests may be run by `+test` sbt command.

To test macros in your project inside the toolbox you may copy and adopt the sources of `tms-test-base` subproject or 
call macros passing `Debug` and/or `Trace` options to macro call directly, with `tmsOptions` annotation, 
environment variables and/or system properties.

Additionally, it is possible to enable "Embedded Fors Code View" tms option to add the code generated by macro  
to macro output as a local string value. This option allows you to view the `for`s stack representation of the macro output 
while using standard scalac option `-Ymacro-debug-lite` or to analise by test code the macro output run in the toolbox. 


## Shortly how it works
The implementation is fully immutable (not counting Scala 2 reflection).

The most general explanation is:
the set of compile time implicits makes possible Transparent Monads (tms) syntax giving access to any inner value of 
the monad (or of the stack of monads) designating the tms access and dropping top stack types. 
Macros replace `ttsNxxx` implicit calls of the input code to the inner values of monads which are extracted in 
the built `for`s stack of the passed type `O`.  

### Transparent Monads syntax by implicits
Built-in implicits satisfy typechecker and designate tms-values and tms-expressions with dropped types and 
access depth levels of the required inner values. 

The general `ttsN` implicits set is the following:
```scala
@compileTimeOnly(ctom) implicit def tts1[T1[_] <: AnyRef, T](t: T1[T]): T = ???
@compileTimeOnly(ctom) implicit def tts2[T1[_] <: AnyRef, T2[_] <: AnyRef, T](t: T1[T2[T]]): T = ???
@compileTimeOnly(ctom) implicit def tts3[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T](t: T1[T2[T3[T]]]): T = ???
@compileTimeOnly(ctom) implicit def tts4[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T](t: T1[T2[T3[T4[T]]]]): T = ???
@compileTimeOnly(ctom) implicit def tts5[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T5[_] <: AnyRef, T](t: T1[T2[T3[T4[T5[T]]]]]): T = ???
```
`AnyRef` upper bound is required to make the implicits priority order in this set. `tt5` has higher priority and 
is applied first during resolving implicits overloads, thus if stacked types have the same accessor overload then 
this accessor is applied to the value of the most inner type of the stack which has such overload.

Other bundled `ttsNxxx` implicits make support for Scala features realised by Predef and primitives conversion implicits, 
includes the set of `identity` implicits for different types to have proper order of implicits applying.

In the tms source code the stack sets of `tts1xxx`-`tts5xxx` implicits are built by macro annotations like in the following example:
```scala
@replaceByTtsImplicits @compileTimeOnly(ctom) def wrapString[T <: String](s: T): WrappedString
```
results
```scala
@compileTimeOnly(ctom) implicit def tts1wrapString[T1[_] <: AnyRef, T <: String](s: T1[T]): WrappedString = ???
@compileTimeOnly(ctom) implicit def tts2wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T <: String](s: T1[T2[T]]): WrappedString = ???
@compileTimeOnly(ctom) implicit def tts3wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T <: String](s: T1[T2[T3[T]]]): WrappedString = ???
@compileTimeOnly(ctom) implicit def tts4wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T <: String](s: T1[T2[T3[T4[T]]]]): WrappedString = ???
@compileTimeOnly(ctom) implicit def tts5wrapString[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T5[_] <: AnyRef, T <: String](s: T1[T2[T3[T4[T5[T]]]]]): WrappedString = ???
```
All implicits may be used explicitly (even omitting types) to change default depth of the inner value to be reached.  
*Transparent TypeS*: "types" because ones are still not a monads at this level and any one-parameter type may be caught by these implicits.

### Transparent Monads macro
Let's use simple example. Imagine we want to add long value to each option integer of the passed list. 
The result will be of `List[Option[Long]]` type:
```scala
import sands.sugar.tms.TransparentMonads._

def addValue(list: List[Option[Int]], value: Long): List[Option[Long]] =
  // if you want to see debug output of the macro while compilation use tmsFor[List[Option[Long]]]("Debug") call of add @tmsOptions("D") annotation to method
  tms[List[Option[Long]]] {  
    value + list // for `list + value` the result will be the same
  } 
```
1) `tts` implicits will be applied to the input code `value + list`. Typechecked code at the macro input will look like
```scala
    value.+(sands.sugar.tms.TransparentMonads.tts2intIdentity[List, Option, Int](list)) // of type Long = yield type
```
2) Scala tree of the passed expression or block is parsed to the internal TmsTree AST. It accepts full Scala syntax available in the Block.
3) Preprocessing is done to drop types of some implicit operations on String which conflicts with native string methods. 
   During preprocessing the nested `ttsNxxx` calls are flattened to one. If "Predef Compliance" option is enabled then 
   the `ttsNxxx` implicit calls with `.+(s: String)` accessor are replaced by Predef `any2stringadd` calls
4) Extraction of `ttsNxxx` implicits and lifting TmsTree nodes to partially untypechecked Scala tree transforms TmsTree to 
   TmsExtracted AST which is the skeleton of the `for`s stack. Each `ttsNxxx` implicit has exactly 1 parameter 
   (where Transparent Monads expression or value is passed) and list of dropped types that represent the part of
   types stack of the Transparent Monads.
   This list of dropped types (in our case it is `List, Option`) determines the `for`s in which Transparent Monads expression should be extracted.
   Macro extracts the expression or value passed to `ttsNxxx` call in `for`s stack and replaces the `ttsNxxx` call by the value obtained in the last `for`.
5) Postprocessing optionally flattens result and optimizes the output `for`s stacks (inlines not used directly Monadic Flow Type vals, drops degenerate `for`s, etc.).
6) Since the output code built with for-comprehensions will be desugared to `flatMap`/`map` while the final typecheck,
   macro builds fors-code view representation of the output code for easy reading and understanding 
   when it is requested by options "Debug" or "EFCV".
7) Lifting of the `for`s stack Scala tree on TmsExtracted AST and untypechecking definitions.

In out case we will have the following fors-code view at the macro output:
```scala
{
  for {
    valueOfList$macro$1 <- list
  } yield {
    for {
      valueOfOption$macro$2 <- valueOfList$macro$1
    } yield {
      value.$plus(valueOfOption$macro$2)
    }
  }
}    
```
So, Transparent Monads macro takes the expression or the Block of statements and pointwise replaces monadic values by the inner ones obtained in `for`s stack. 
After replacing `ttsNxxx` calls by the inner values the entire passed code becomes `yield` part of the built `for`s stack.

### Monadic Flow Control macro
In addition to Transparent Monads macro transformations, the Monadic Flow Control macro at the above "extraction" step 
treats the passed Block of code (single passed expression will be wrapped to the Block) as having Monadic Flow Control 
relations between statements. 

Actually it translates the passed sequence of statements to one (of the stacked) for-comprehension of Monadic Flow Type 
which is detected by the first statement.

Macro detects stable val definitions, groups expressions of non-Monadic Flow Type, separates tailing yield statements, 
does definitions scope validation and usage possibility, and then maps that parts to `for` `<-` or `=` enums and `yield`. 

Each separate part of input statements is added to the `for`s stack after tms-extraction of its expressions. This means that 
Monadic Flow statement may also contain tms-values inside its expressions, but these tms-values (or tms-expressions) 
may only be extracted in the top of `for`s stack that precedes Monadic Flow Type (inclusively). Tms-expressions of 
the `yield` part of the mfc-block may be extracted in the entire types stack.

For example, we need to duplicate the string pattern `n/m` integer times with logging. We have the pattern as `Try[String]`:
```scala
import sands.sugar.tms.TransparentMonads._

def log(v: Any): Unit = {}

@tmsOptions$options
def patternTimes(n: Int, m: Int, pattern: Try[String]): Try[String] = mfc[Try[String]] {
  val times = Try(n / m) // Monadic Flow Type is Try
  log(s"$n/$m=")           // the group of 2 non Monadic Flow Type expressions:
  log(times)
  val len = pattern.length // non Monadic Flow Type stable val & the last val in the block
  // yield starts here
  log(len)                 
  pattern * times
}
```
The typechecked code of macro input after implicits applying is:
```scala
{
  val times = scala.util.Try.apply[Int](n./(m));
  log((("".+(n).+("/").+(m).+("=")): String));
  log(times);
  val len = sands.sugar.tms.TransparentMonads.tts1stringIdentity[scala.util.Try, String](pattern).length();
  log(len);
  sands.sugar.tms.TransparentMonads.tts1augmentString[scala.util.Try, String](pattern).*(sands.sugar.tms.TransparentMonads.tts1intIdentity[scala.util.Try, Int](times))
}
```
The macro output code with Monadic Flow Control block mapped to the `for` is:
```scala
{
  val times = scala.util.Try.apply[Int](n./(m))
  for {
    timesValue$macro$1 <- times       // `name <-` for all Monadic Flow Type stable vals 
    wcNonMf$macro$2 = {               // `_ =` for groups of statements of non Monadic Flow Type 
      log(("".+(n).+("/").+(m).+("="): String));
      log(times)
    }
    valueOfTry$macro$3 <- pattern     // this is a tms extraction of String value of Try[String] pattern  
    len = valueOfTry$macro$3.length() // `name =` for all non Monadic Flow Type stable vals 
  } yield {
    log(len);
    valueOfTry$macro$3.$times(timesValue$macro$1)
  }
}
```
Monadic Flow Type may be any type of resulting type `O` of the monadic stack: its `for` may be any one of the built `for`s stack. 
When types stack contains several types for which Monadic Flow Type conforms to then the first outer one becomes Monadic Flow Control `for`.    

### Flattening the result
Both macros may flatten the result of built `for`s stack. 

When the resulting `yield` block (or expression) has the same type as the most inner type of the built `for`s stack then 
the result may be flattened. Flattening is controlled by the passed to macro result type `O`. If the `yield` block type 
conforms to the last *used* in `for`s stack type then result will be flattened when the yield type *does not* conform 
to the next *not used* inner type closest to that *used* type.     

In other words, if we define inside the stack type `O` additional inner type that is (confirmed to) the `yield` type then
it will not be flattened but will be filled by the yielding block (or expression).

For example, to calculate the following tms-expression:
```scala
Some( Some(1) + 2 )
```
we may specify two variants of the resulting type in the macro call:
```scala
tms[Option[Int]](Some( Some(1) + 2 ))
// that is built to 
{
  for {
    valueOfSome$macro$1 <- scala.Some.apply[Int](1)
    flatMappedValueOfSome$macro$2 <- scala.Some.apply[Int](valueOfSome$macro$1.$plus(2))
  } yield {
    flatMappedValueOfSome$macro$2
  }
}
```
or
```scala
tms[Option[Option[Int]]](Some( Some(1) + 2 ))
// that is built to 
{
  for {
    valueOfSome$macro$1 <- scala.Some.apply[Int](1)
  } yield {
    scala.Some.apply[Int](valueOfSome$macro$1.$plus(2))
  }
}
```


## Limitations and notes

### Type variables (type classes) and tts implicits
Due to the inner implicits stack priority (any `tts5xxx` has higher priority than any `tts1xxx` implicit has 
to reach overloaded accessor of the inner type first) implicits have `AnyRef` upper bound for transparent type parameters 
that forms types stack. That is why to be applied to type variables latter should be defined with `AnyRef` upper bound. 
Like in
```scala
  def sample[M <: AnyRef : Typeclass]() =
    /* implicit */ tts1[M, Int](...)
```
This is required only when any `ttsNxxx` implicit should be applied to the stacked type that contains type variable `M` 
(when `M` should be a type parameter of `ttsNxxx[.., M, ...](...)` call). In other words, when type parameter is the part of 
the "opened" types stack of the tts-value.
When type variables (thus type classes) are not accessed as tms-values then upper bound may be omitted. 
Please, keep in mind that missed implicit applying may not be controlled or verified and thus may result in skipping 
tms interpretation of the value and lead to non-clear errors.   


### Array collections overload precedence
Most accessors of `Array[T]` collections are implemented by Predef implicit extensions. 
Implementing ones `tts1generic...`-`tts5generic...` stack companions of `genericArrayOps` and `genericWrapArray` 
leads to `Array[T]` collections have more priority on implicit overloads resolving over other collections.
In practice this leads to the following: if `Array` type is outer than other collection type (let the `Seq` type) 
in the types stack then the `Array` overloaded accessor of both types will be used first (contrarily to Seq accessor 
"should" be used by general priority rule). So `Option[Array[Seq[Int]]].length` will result `Option[Int]` getting 
the optional length of an `Array` contrarily to expected (by general rule) optional array of sequences length `Option[Array[Int]]`.

### Call by name caution
The way used to extract inner values in the embracing `for`s stack breaks call-by-name feature when tms-value 
(or tms-expression) is passed to call-by-name parameter. By the current implementation such tms- or 
tms-dependent expression should be evaluated (or partially evaluated) in the embracing `for` thus not inside 
the function it is passed to. This limitation looks irresistible. For example, in:
```scala
  tms[Option[Boolean]] {
    true || Option(false)
  } 
  // built as
  {
    for {
      valueOfOption$macro$1 <- scala.Option.apply[Boolean](false)
    } yield {
      true.$bar$bar(valueOfOption$macro$1)
    }
  }
```
`Option(false)` will be evaluated first in the embracing `for`s stack and will be passed to `.||` call-by-name parameter 
as a value unlike usually expected behavior when `Option(false)` is not evaluated at all (without tms).

When expression passed to call-by-name parameter partially depends on the tms-expression (or tms-value) then such tms-part 
will be evaluated first, resulting the expression passed to call-by-name parameter is partially evaluated even when not used. 

When tms option for building inner fors stacks for apply parameter are used then expressions that partially depend on tms 
will be evaluated completely before passing to call-by-name parameter.

### Evaluation order
Resulting evaluation flow is determined by the order of tms-expressions extraction. Tms-expressions are added to `for`s stack 
for inners extraction in the order ones appear in the usual evaluation flow of the input block or expression. 

For composite expressions like `if`, `match`, `try`, etc. with several flow branches, the branches are processed in the order ones appear in definition.
I.e. for the `if` statement (actually: composite expression that consists of expressions):
- predicate expression
- true-branch expression
- false-branch expression

For `transparentMonads` the `yield` (actually: entire passed block) becomes the function of all tms-expressions evaluated 
in the embracing `for`s. Thus, all inner values of all tms expressions are evaluated in `for`s stack first then 
`yield` block is evaluated as usual having that inner values.  

`monadicFlowControl` macro has mixed evaluation order since the initial statements of mfc-block are mapped to 
the one `for`-enums inside `for`s stack. The order looks like:
- `for`s that correspond to the types outer than Monadic Flow Type (if any) 
- Monadic Flow Type `for` with mfc-expressions mapped to its enums
- `for`s that correspond to the types inner than Monadic Flow Type (if any)

This order also limits tms-expressions used in the init part of mfc-block (not `yield` part) to be extracted only 
for types that are outer than Monadic Flow Type (inclusively). In other words, to "open" such tms-expression for getting 
inner value only types of the stack above (or including) Monadic Flow Type may be dropped. 
`for`s inner than mfc-`for` may not be used for ones extraction.

It is more or less simple when all used tms-expressions only have monads of the same one type. 

If monads stack is used then we have the following picture. 
Tms-expressions are evaluated partially in the order of types in the stack (in the order of `for`s in the built stack):
first, outer types of the stack are evaluated, then inner types. So tms expression are evaluated not one-by-one completely 
but in the "across" "by type" mode depending on dropped types of ones.

For example, we have 2 tms expressions `Try(Some(0) + 1)` and `Some(2) + Try(3)` appeared in the specified order 
as parts of the input code for resulting types stack `Option[Try[Int]]`. The order in which ones are evaluated is:
- `s1 <- Some(0)` outer `for` of `Option`
- `s2 <- Some(2)`
- `t1 <- Try(s1 + 1)` inner `for` of `Try`
- `t2 <- Try(3)`
- `st = s2 + t2` in the place of the last expression usage (`yield` for both macros or value expression for `monadicFlowControl` macro if Monadic Flow Type is `Try`) 

So, all `Option`s of all tms-expressions first then all `Try`s.

To change this "across" tms-expressions evaluation order there are tms options to built subexpressions that depend on tms each in separate inner `for`s stacks. 

### Macro output type and nested tms-expressions 
Resulting macros output type `O` depends on type of the input code and tms-expressions used in it. 
Above tms-expressions extraction order determines the types and order in which these types should exist in the output types stack `O`.     

Special attention is required for tms-expressions or tms-values nested into tms-expression.

Let's take two examples of separate tms-expressions and the nested ones.
Separate is `Try(Some(1)) + Try(Some(2))` and the nested is `Try( Try(Some(1)) + 2 ) + 3`.

`Try(Some(1)) + Try(Some(2))` has two independent `Try(Some(*))` tms-expressions. 
In the `Try(Some(1)) + Try(Some(2))` each `Try(Some(*))` will consume `Try` and `Option` `for`s 
of the stack extracting corresponding inner `Int` value and then ints are summarized.
The result type is `Try[Option[Int]]`. Each tms-expression uses `for`s stack for extraction separately starting its top.

Nested one `Try( Try(Some(1)) + 2 ) + 3` has two tms expressions: inner `Try(Some(1)) + 2` that 
consumes `Try` and `Option` `for`s like the first sample does but its extracted int value (let `intValue`) should 
be used inside the second tms-expression `Try( intValue ) + 3`. Now, to extract `Try( intValue )` we should use 
another additional `Try` `for` that is inside already used `for`s stack of `Try`-`Option`.
The first `Try` `for` may not be used since it extracts `intValue`. That is why the result type of `Try( Try(Some(1)) + 2 ) + 3` is
`Try[Option[Try[Int]]]`.

Macros control the usage of requested types stack (requested by the type `O`) during extraction, and reports an error 
when required type is absent in a non-consumed rest of the stack at the point of extraction. 

### Local definitions limitation
- for `transparentMonads` macro: any type of local definition may not be used as tms or inside tms-expression
- for `monadicFlowControl` macro: local stable values defined in the root block of passed code may be used as tms or 
  inside tms-expressions but definitions other than stable vals may occur only after the last stable val definition or 
  Monadic Flow Type expression (only be ones that go to the resulting `yield` part)

Those are embracing `for`s stack implementation limitations.

The reason of stable vals limitation of `transparentMonads` is: to reach the inner value of tms it should be used in 
embracing `for`s stack that apriori is out of the val definition scope.

For other types of definition of `monadicFlowControl`: ones do not have for-comprehension equivalents like stable vals have. 

### Implicits scope workaround
When you encounter the "compile time only" compiler error on `tts` implicits call then this primarily mean 
that `ttsNxxx` implicit is applied outside the macros `tmsCode` passed. The workaround is to move import 
of `sands.sugar.tms.TransparentMonads._` closer to the macro call to shrink implicits scope up to the call only.

The found reasons are:
- some scalatest (or other packages) implicits behaviour may be overridden by tts implicits  
- predef any2stringadd feature syntax interfere with tms. Inside passed tmsCode it is corrected by macro 
  (standard Scala behaviour is recovered or ignored depending on "Predef Compliance" option) but outside the macro call nothing does this.

### 2.11 lazy vals scope is not verified
Support of `lazy val` in 2.11 reflection generates tree that makes hard to validate and work with the ones symbols. 
The incorrect scope usage check are not implemented for 2.11 `lazy val` but compiler will emit corresponding error. 

### IDEA plugin syntax checks
Unfortunately, implicits highlighting and based errors check by the IDEA plugin continue to surprise with ignorance 
of the complete Scala implicits knowledge. Looks like there is something insurmountable in complex expressions with implicits. 
This project is very sensitive to that knowledge by instruments thus some highlighted errors are wrong in the IDEA. 

Please, try to compile code first before treating highlighted syntax "error" as an actual error.
To prove some controversial point the explicit `ttsN` calls may also be used.


## Tms Options
Options passed to environment variable or system property should be separated by ';'.
Options passed to `@tmsOptions()` annotation or directly to `*For()()` call variants should be a list of literal strings and may be empty.
Each literal string may contain separate option or its abbreviation (both ignoring case), or a list of ones separated by ';'.
All following option lists have the same effect:

        "debug", "No trace", "No Predef Compliance", "embedded fors code view"
        "D;NT", "NPC;Embedded Fors Code View"
        "d;nt;npc;efcv"

Incorrect option will be reported as an error. 
When directly specified pre-evaluation type is not found in the `O` types stack of the macro call then an error is raised too.

### Options summary

| Options | Abbreviations | Description |
|---|---|---|
| "Debug" / "No Debug"                                                             | "D" / "ND"            | enables / disables macros debug output |
| "Trace" / "No Trace"                                                             | "T" / "NT"            | enables / disables macros trace output |
| "Predef Compliance" / "No Predef Compliance"                                     | "PC" / "NPC"          | enables / disables any2stringadd predef compliance. When enabled `Some(1) + "2"` is `"Some(1)2"`, when disabled - `Some("12")` |
| "Fors Stack For Apply Source" / "No Fors Stack For Apply Source"                 | "FSFAS" / "NFSFAS"    | enables / disables building apply source containing ttsN in a separate inner Fors Stack |
| "Fors Stack For Apply Parameter" / "No Fors Stack For Apply Parameter"           | "FSFAP" / "NFSFAP"    | enables / disables building apply parameter containing ttsN in a separate inner Fors Stack |
| "Embedded Fors Code View" / "No Embedded Fors Code View"                         | "EFCV" / "NEFCV"      | enables / disables embedding of the string containing fors-view of the macro output to macro result as first local val in the output block |
| "Single Fors Stack"                                                              | "SFS"                 | equivalent to applying "No Fors Stack For Apply Source" and "No Fors Stack For Apply Parameter" options at the same time |
| "All Fors Stacks"                                                                | "AFS"                 | equivalent to applying "Fors Stack For Apply Source" and "Fors Stack For Apply Parameter" options at the same time |
| "Pre Evaluate No Types"                                                          | "PENT"                | disables pre evaluation for all Fors Stack types |
| "Pre Evaluate All Types"                                                         | "PEAT"                | enables pre evaluation for all Fors Stack types |
| "Pre Evaluate Monadic Flow Type"                                                 | "PEMFT"               | for *mfc()/mfcFor()()* macro calls it enables pre evaluation of the detected Monadic Flow Type |
| "Pre Evaluate *class_name/FQN*, ..."                                             | "PE *cn/FQN*, ..."    | adds the list of class names or fully qualified names to the set of pre evaluated types. For example, `"PE Future, scala.util.Try"` |

### Order of applying

- *default* hardcoded options

        "No Debug"
        "No Trace"
        "Predef Compliance"
        "No Embedded Fors Code View"
        "Single Fors Stack"
        "Pre Evaluate No Types"

- options from environment variable *defaultTmsOptions* (if present) separated by ';', for example

        D;T

- options from system property *defaultTmsOptions* (if present) separated by ';', for example, passed to sbt as

        -DdefaultTmsOptions="Debug;Trace" // quated when string contains ';'

- options from *@tmsOptions()* annotations (if present) starting outer enclosing symbol of macro call to inner ones, for example

        @tmsOptions("No Debug", "NT", "Pre Evaluate All Types") 
        class Test // containig tms/mfc macro calls

- options passed to *tmsFor()() or mfcFor()()* macro calls as the first group of parameters, for example

        tmsFor[Option[Int]]("Debug; PE Option") {
          Some(1) + 2
        }

- options from environment variable *overrideTmsOptions* (if present) separated by ';', for example

        Debug;NT;PENT

- options from system property *overrideTmsOptions* (if present) separated by ';', for example, passed to sbt as

        -DoverrideTmsOptions="Debug;NT;PENT" // quated when string contains ';'

Any option may be changed at any level of the above "tree" overriding option value collected at previous steps.
This makes possible to specify options for the groups of macro calls enclosed by, for example, a class, specify default
or override options for all macro calls in the project by environment variable or system property, 
or set preferred options for each macro call separately.

### Pre-evaluation
This set of options enables building of pre-evaluation vals of the specified (or of all) types before the `for` 
corresponding to that type(s). 
Each expression of the `for` arrow enums is pre-evaluated before `for` until it depends on previous `for`-vals. 
We often use this technique to start several `Future`s in parallel before using them in `for`. 

Fo example, enabling of the pre-evaluation in the following:
```scala
tmsFor[Future[Int]]("PE Future") {
  Future(1) + Future(2)
}
```
results the macro output:
```scala
{
  val valFuture$macro$1 = scala.concurrent.Future.apply[Int](1)(ec)
  val valFuture$macro$3 = scala.concurrent.Future.apply[Int](2)(ec)
  for {
    valueOfFuture$macro$2 <- valFuture$macro$1
    valueOfFuture$macro$4 <- valFuture$macro$3
  } yield {
    valueOfFuture$macro$2.$plus(valueOfFuture$macro$4)
  }
}
```
If directly specified type of this option is absent in passed types stack then macro reports an error to prevent misprints.
Short name or FQN (or both) may be passed to this option. 

The group of options to control pre-evaluation is: 

    "Pre Evaluate No Types"
    "Pre Evaluate All Types"
    "Pre Evaluate Monadic Flow Type"
    "Pre Evaluate <class_name>|<FQN>[, <class_name>|<FQN>]+"

Directly specified types in the stack of options (external, of enclosing annotations and directly passed to macro call) are accumulated. 
By default, pre-evaluation is off: "Pre Evaluate No Types".

### Inner Fors Stacks to control evaluation Order
As noted above, by default the order of evaluation of tms-expressions that have stacked type of 2 or more monads becomes 
the "across" like "by-type" order: first, the parts of outer type are evaluated in the outer `for`, next - of the next 
inner monad type in the next `for` and so on.

Two options "Fors Stack For Apply Source" and "Fors Stack For Apply Parameter" 
make possible the complete evaluation of one tms-expression before evaluation of the next one with the same stacked type.

These options influence the order of evaluation of tms-expressions that are represented by `.apply` tree.

Using terminology `apply_source.apply(apply_parameters)` the following expression:
```scala
Some(1) + Some(2) + 3
```
has "apply sources":
- `Some`
- `Some(1)`
- `Some`
- `Some(1) + Some(2)`

and "apply parameters":
- `1`
- `2`
- `Some(2)`
- `3`

When such option for apply-source or apply-parameter is enabled then expressions being the apply-source or passed to 
the apply-function as parameters are evaluated in the inner for stacks of the `O` type when the following condition is met:
tms-expression is not a solid tms but depends on tms-subexpression.

When expression does not contain tms inside or is just solid tms value (wrapped to `ttsNxxx` call without inner tms) then 
building it in separate `for`s stack has no sense - `for`s stack will be degenerate.

In the above example only `Some(1) + Some(2)` apply source meets this condition:
```scala
tmsFor[Option[Int]]("FSFAS") {
  Some(1) + Some(2) + 3
}
```
builds
```scala
{
  for {
    valueOfOption$macro$3 <- {
      for {
        valueOfSome$macro$1 <- scala.Some.apply[Int](1)
        valueOfSome$macro$2 <- scala.Some.apply[Int](2)
      } yield {
        valueOfSome$macro$1.$plus(valueOfSome$macro$2)
      }
    }
  } yield {
    valueOfOption$macro$3.$plus(3)
  }
}
```
Example of "apply parameter"
```scala
tmsFor[Option[Int]]("FSFAP") {
  Some(1) + (Some(2) + 3)
}
```
builds
```scala
{
  for {
    valueOfSome$macro$1 <- scala.Some.apply[Int](1)
    valueOfOption$macro$3 <- {
      for {
        valueOfSome$macro$2 <- scala.Some.apply[Int](2)
      } yield {
        valueOfSome$macro$2.$plus(3)
      }
    }
  } yield {
    valueOfSome$macro$1.$plus(valueOfOption$macro$3)
  }
}
```

These options guarantee that in the case first evaluated tms-expression fails (or empty, etc.) then the second tms-expression 
will not be evaluated even partially (by its types stack).

More complex example with two types in the stack & inner `for`s stacks building for both apply-sources & apply-parameter:
```scala
tmsFor[Try[Option[Int]]]("All Fors Stacks") {
  (Try(Some(1)) + 2)*(Try(Some(3)) + 4)
}
```
generates
```scala
{
  for {
    valueOfTry$macro$3 <- {
      for {
        valueOfTry$macro$1 <- scala.util.Try.apply[Some[Int]](scala.Some.apply[Int](1))
      } yield {
        for {
          valueOfSome$macro$2 <- valueOfTry$macro$1
        } yield {
          valueOfSome$macro$2.$plus(2)
        }
      }
    }
    valueOfTry$macro$7 <- {
      for {
        valueOfTry$macro$5 <- scala.util.Try.apply[Some[Int]](scala.Some.apply[Int](3))
      } yield {
        for {
          valueOfSome$macro$6 <- valueOfTry$macro$5
        } yield {
          valueOfSome$macro$6.$plus(4)
        }
      }
    }
  } yield {
    for {
      valueOfOption$macro$4 <- valueOfTry$macro$3
      valueOfOption$macro$8 <- valueOfTry$macro$7
    } yield {
      valueOfOption$macro$4.$times(valueOfOption$macro$8)
    }
  }
}
```
Built degenerate `for`s are optimized while postprocessing.

These options are applicable only to the solid expressions based on the tree of `.apply`s.
Thus, for instance, `if` composite expression does not follow this options as a whole, only its predicate & branch expressions 
may do but independently and when ones are `apply` tree based.

These options also additionally influence the ability to use local definitions as a part of tms-expressions.
If any local definition identifier (val, lazy val, var, type, class, object, def) is the part of tms-expression 
(is not a tms itself), for example:
```scala
val localVal = 2
Try[Int](Some(1) + localVal) // here `Some(1) + localVal` is a tms-expression due to `Some(1)` is used as Transparent Monads to add the Int   
```
then it will be compiled correctly with "Fors Stack For Apply Parameter" option disabled, but will fail when this option is enabled.

When "Fors Stack For Apply Parameter" is enabled then the whole expression `Some(1) + localVal` passed as parameter to `Try[Int].apply` 
should be extracted in the inner `for`s stack outside `localVal` definition and `transparenMonads` macro emits an error.

When "Fors Stack For Apply Parameter" is disabled then only `Some(1)` tms expression should be extracted in 
the embracing `for`s stack and no error occurs. 

"Fors Stack For Apply Parameter" option may be enabled when this sample code with `val localVal = Some(2)` 
is passed to `monadicFlowControl` macro but this rule works only for stable val definitions and when the stack types 
outer than the Monadic Flow Type are not used in that tms-expression. 

Using definitions of other types (not a stable vals) as part of tms-expression in source or parameter position with
"Fors Stack For Apply Source" or "Fors Stack For Apply Parameter" option enabled results in an error for both macros.

The group of options that controls inner `for`s stacks building is:

    "Single Fors Stack"
    "Fors Stack For Apply Source" / "No Fors Stack For Apply Source"
    "Fors Stack For Apply Parameter" / "No Fors Stack For Apply Parameter"    
    "All Fors Stacks"

By default, building of the inner `for`s stacks is disabled: "Single Fors Stack".

### Predef compliance (any2stringadd)
`ttsNxxx` implicits reacts to syntax implemented by `any2stringadd` Predef implicit and interprets it as tms syntax.

To prevent this behaviour macros replace such `ttsNxxx` wrapping of Any values to Predef `any2stringadd` method keeping 
predef compliance.

The option "Predef Compliance" controls this macro's behaviour.

In the example:
```scala
tmsFor[Try[String]]() {
  Try("1") + "2"
}
```
the output will be (resulting no tms at all):
```scala
{
  any2stringadd(scala.util.Try.apply[String]("1")).$plus("2")
}
```
When option is disabled:
```scala
tmsFor[Try[String]]("No Predef Compliance") {
  Try("1") + "2"
}
```
the input will be treated as tms expression:
```scala
{
  for {
    valueOfTry$macro$1 <- scala.util.Try.apply[String]("1")
  } yield {
    valueOfTry$macro$1.$plus("2")
  }
}
```
By default, `any2stringadd` Predef compliance is enabled: "Predef Compliance".

### Helper options
"Debug" and "Trace" options enable the tracing of the macro workflow, printing the input & output trees and 
code representations, tracing of the inner AST parsing & tms extraction, tracing of definition symbols, etc.

The full debug & trace output of the last example is the following:
```log
[info] * tms >>> debug/trace of macro processing
tmsFor[Try[String]]("No Predef Compliance", "D", "T") {
 ^ .../TmsMacro.sc:15:55
[debug] * tms TmsOptions: debug=true, trace=true, monadicFlowControl=false, forsStackForApplySource=false, forsStackForApplyParameter=false, preEvaluateTypes=Set(), predefCompliance=false, embeddedForsCodeView=false
[debug] * tms INPUT code.tree: sands.sugar.tms.TransparentMonads.tts1stringIdentity[scala.util.Try, String](scala.util.Try.apply[String]("1")).+("2")
[debug] * tms INPUT showCode(code.tree): sands.sugar.tms.TransparentMonads.tts1stringIdentity[scala.util.Try, String](scala.util.Try.apply[String]("1")).+("2")
[debug] * tms INPUT raw Expr: Expr(Apply(Select(Apply(TypeApply(Select(Select(Select(Select(Ident(sands), sands.sugar), sands.sugar.tms), sands.sugar.tms.TransparentMonads), TermName("tts1stringIdentity")), List(TypeTree(), TypeTree())), List(Apply(TypeApply(Select(Select(Select(Ident(scala), scala.util), scala.util.Try), TermName("apply")), List(TypeTree())), List(Literal(Constant("1")))))), TermName("$plus")), List(Literal(Constant("2")))))
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(sands,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(sands.sugar,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(sands.sugar.tms,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(sands.sugar.tms.TransparentMonads,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(scala,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(scala.util,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(scala.util.Try,true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree("1",true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree("1",true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree(scala.util.Try.apply[String]("1"),true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsApply(RangePosition(<console>, 58, 61, 66),String @@ sands.sugar.tms.TransparentMonads.TmsStringImplicitExtensions,Some(TmsOtherTree(sands.sugar.tms.TransparentMonads,true)),tts1stringIdentity,None,List(scala.util.Try, String),List(List(TmsOtherTree(scala.util.Try.apply[String]("1"),true))))
[trace] * tms syntax tree: Unlifted TmsExpression = TmsOtherTree("2",true)
[trace] * tms syntax tree: Unlifted TmsExpression = TmsApply(RangePosition(<console>, 58, 67, 72),String,Some(TmsApply(RangePosition(<console>, 58, 61, 66),String @@ sands.sugar.tms.TransparentMonads.TmsStringImplicitExtensions,Some(TmsOtherTree(sands.sugar.tms.TransparentMonads,true)),tts1stringIdentity,None,List(scala.util.Try, String),List(List(TmsOtherTree(scala.util.Try.apply[String]("1"),true))))),$plus,None,List(),List(List(TmsOtherTree("2",true))))
[trace] * tms input code definition Symbols: none
[trace] * tms fors stack extraction: Created new TmsForsBuilder = TmsForsBuilder(0,List(TmsForCollector(scala.util.Try[String],List(),List())))
[trace] * tms fors stack extraction: Building Fors Stack monadic value extraction for tts types [Try] of expression = scala.util.Try.apply[String]("1")
[trace] * tms Definition Symbols of TmsExtractedTree(scala.util.Try.apply[String]("1")): none
[trace] * tms fors stack extraction: extractMonadicValue: Added For Arrow Enum = 'valueOfTry$macro$1 <- TmsExtractedTree(scala.util.Try.apply[String]("1"))'
[debug] * tms extracted fors code view:
{
  for {
    valueOfTry$macro$1 <- scala.util.Try.apply[String]("1")
  } yield {
    valueOfTry$macro$1.$plus("2")
  }
}
[debug] * tms postprocessed fors are unchanged
[trace] * tms syntax tree: Setting NoSymbol to definition tree = ValDef(Modifiers(PARAM), TermName("valueOfTry$macro$1"), TypeTree(), EmptyTree)
[debug] * tms OUTPUT code.tree: scala.util.Try.apply[String]("1").map(((valueOfTry$macro$1) => valueOfTry$macro$1.$plus("2")))
[debug] * tms OUTPUT showCode(code.tree): scala.util.Try.apply[String]("1").map(((valueOfTry$macro$1) => valueOfTry$macro$1.+("2")))
[debug] * tms OUTPUT raw Expr: Expr(Apply(Select(Apply(TypeApply(Select(Select(Select(Ident(scala), scala.util), scala.util.Try), TermName("apply")), List(TypeTree())), List(Literal(Constant("1")))), TermName("map")), List(Function(List(ValDef(Modifiers(PARAM), TermName("valueOfTry$macro$1"), TypeTree(), EmptyTree)), Apply(Select(Ident(TermName("valueOfTry$macro$1")), TermName("$plus")), List(Literal(Constant("2"))))))))
```
Just sometimes helps to answer the question to yourself: "what are you doing, dude?" :smile: and saves time as usual.

The output is written to scalac stdout by `println`.

"Embedded Fors Code View" is another helpful option that embeds the output code represented by `for`s stack as 
a local string of the output code. It maybe used (but still not used) in tests withing toolbox for parsing and 
comparing results with expected or for usage with standard scalac debug options.

When this option is enabled and scalac has "-Ymacro-debug-lite" option passed then the scalac output of the last example 
will look like (run in worksheet):
```log
performing macro expansion sands.sugar.tms.TransparentMonads.tmsFor[scala.util.Try[String]]("NPC", "EFCV")(sands.sugar.tms.TransparentMonads.tts1stringIdentity[scala.util.Try, String](scala.util.Try.apply[String]("1")).+("2")) at RangePosition(<console>, 0, 35, 55)
{
  val forsCodeView$macro$2 = "\n{\n  for {\n    valueOfTry$macro$1 <- scala.util.Try.apply[String](\"1\")\n  } yield {\n    valueOfTry$macro$1.$plus(\"2\")\n  }\n}\n";
  scala.util.Try.apply[String]("1").map(((valueOfTry$macro$1) => valueOfTry$macro$1.$plus("2")))
}
Block(List(ValDef(Modifiers(), TermName("forsCodeView$macro$2"), TypeTree(), Literal(Constant("
{
  for {
    valueOfTry$macro$1 <- scala.util.Try.apply[String]("1")
  } yield {
    valueOfTry$macro$1.$plus("2")
  }
}
")))), Apply(Select(Apply(TypeApply(Select(Select(Select(Ident(scala), scala.util), scala.util.Try), TermName("apply")), List(TypeTree())), List(Literal(Constant("1")))), TermName("map")), List(Function(List(ValDef(Modifiers(PARAM), TermName("valueOfTry$macro$1"), TypeTree(), EmptyTree)), Apply(Select(Ident(TermName("valueOfTry$macro$1")), TermName("$plus")), List(Literal(Constant("2"))))))))
val res1: scala.util.Try[String] = Success(12)
```
The group of helper options is:

    "Debug" / "No Debug"
    "Trace" / "No Trace"
    "Embedded Fors Code View" / "No Embedded Fors Code View"

By default, helper options are: "No Debug", "No Trace", "No Embedded Fors Code View".


## Contributing
Please, feel yourself free (in terms of [LICENSE] :smile: ) to use, polish & continue project, report & fix bugs, 
research & build theories, ask the questions, discuss and get the pleasure of its further development & thinking up.

Implement your own ideas based on the ready code base or add the new one here independently using the tests base to verify. 
Later (if it will grow and find the followers & users) we may make the project the part of some virtual organization on GitHub.

All repo related moments (bugs, questionable functionality, code suggestions, doc corrections, etc.: anything that may lead
to the clear task to be done) are in [Issues].

Questions and discussions of current & new ideas, ways to implement ones, restrictions and possible solutions, theory, etc.
are in [Discussions].

See [CONTRIBUTING.md] for more details (or in the root of repo or artifact).

Hope these ideas with the opportunity to play realizations will give some good evenings and a pleasant thinking.


## Spacial thanks to
- authors and implementers of the beautiful Scala language
- contributors & future one-thinkers this project will meet
- 20 min. video that frees 20% of the brain (of monadic intuition) for dummies like me who started
  with assembler/C :smile: [How the flow control monad was born]


## Versions
| Number | Released | Changes |
|---|---|---|
| 0.2.4 | July, 2021 | initial release |

[tms and mfc article]: https://github.com/SerhiyShamshetdinov/sugar-tms/blob/main/tms_and_mfc.md
[LICENSE]: https://github.com/SerhiyShamshetdinov/sugar-tms/blob/main/LICENSE
[Issues]: https://github.com/SerhiyShamshetdinov/sugar-tms/issues
[Discussions]: https://github.com/SerhiyShamshetdinov/sugar-tms/discussions
[CONTRIBUTING.md]: https://github.com/SerhiyShamshetdinov/sugar-tms/blob/main/CONTRIBUTING.md
[How the flow control monad was born]: https://www.youtube.com/watch?v=t1e8gqXLbsU&t=2s
