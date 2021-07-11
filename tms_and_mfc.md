# Transparent Monads syntax and Monadic Flow Control interpretation (at Scala examples)

Just we have any monadic source of processed data (async, for example) or want to use native parallelism then
everything in the project becomes monadic. At any layer of code: from data sources to responses. Then we add `F[_]` and cats.
Other source of daily used monads is collections. Collections are simply indispensable.

We use high profit from structuring with and safe functionality of monads but at the clarity expense of additional layer 
of manipulations resulting the previously simple and clear business code becomes tangled in the web of `for`s, `flatMap`s 
and `maps`s. Wait 10 year more and companies will be forced to hire code archaeologists along with code architects 
to support big old projects and to understand business logic embedded in the past. :smile:

We run away to the Scala from thousands of nested loops like `for(i = 0; i < n; ++i)`, infinite `null` checks, 
non-suitable syntax and partial libs, but having that pleasure we weave a multistage web of monads manipulations. 
Ones are so habitable and native in daily use that we take it for granted.  

Is it possible to simplify monads manipulation layer or even hide it by putting to the macro or compiler layer, 
evict it from the code we write?

The primary question is: how to reach this simplification? 
The background one: how much (implicit :smile: ) knowledge anyone who reads such simplified code should have to understand it correctly?
In other words: may the understanding of simplified code be based on some intuition that is easier than the understanding 
of monads manipulation layer requires now?

Answers also may give some "springboard" to enter the Scala for non-Scala developers. Something like the for-comprehension does.
The use of the modern technics provided by `F[_]`, cats, ect. solves our internal development questions due to simplicity 
of task solving but also makes the threshold for entering the Scala higher and higher.

This article describes two ideas for such an attempt at simplification. It is based on intuition and a short description
of the research implementation by two macros. Realization is based on the set of implicits and on the mapping of the input code
to the stack of for-comprehensions. Some analysis of constraints is also provided.

Our favorite question is "What for?" :smile:
- to simplify business code definition syntax and to let compiler do the obvious monads manipulations 
- to think about, to deconstruct and to have possibility to define business processes at as simple layer as business has 
  (perfectly: one to one) still being monadic in the code (business does not know what monad is while describing their requirements)


## Table of contents
- [Transparent Monads syntax](#transparent-monads-syntax)
  - [Intuition](#intuition-tms)
  - [Satisfying typechecker](#satisfying-typechecker)
  - [Transparent Monads type approximation](#transparent-monads-type-approximation)
  - [Types Stack overloads resolving order](#types-stack-overloads-resolving-order)
  - [Building embracing Fors Stack](#building-embracing-fors-stack)
  - [Local definitions limitation](#local-definitions-limitation-tms)
- [Monadic Flow Control](#monadic-flow-control)
  - [A couple of abstract reasoning](#a-couple-of-abstract-reasoning)
  - [Intuition](#intuition-mfc)
  - [Local definitions limitation](#local-definitions-limitation-mfc)
- [Conclusion](#conclusion)
- [What to play with](#what-to-play-with)


## Transparent Monads syntax

The general idea of Transparent Monads is simple: what if we let the syntax with direct access to the inner values of the monadic value (or of the stack of monads)
and let the macro to recover required monadic manipulations?

*Transparent Type* `M[T]` may be approximated as having type `M[T] with T` as to accessors reachability and overloads resolution order.
Applied recursively, the stack of *Transparent Types* `M1[M2[M3[T]]]` - as having type `M1[M2[M3[T]]] with M2[M3[T]] with M3[T] with T`.
So, Transparent Types Stack looks like the turned inside out real stacked type.

*Transparent Monad* and *Transparent Monads Stack* - Transparent Type and Transparent Types Stack respectively with 
the only requirement for types `Mn[T]` to have `flatMap` and `map` methods to build ones for-comprehensions without filters.

Monadic Flow Control idea realization of `monadicFlowControl` macro is based on Transparent Monads.

### Intuition {#tms}
How many common methods and properties (accessors) `Try[Int]` and `Int` types have? Or `Option` and any primitive type? 
Any monad and our custom business logic class?

Please, keep in mind that any simple constructor of monadic class in the examples may be replaced by any other function 
with the same (or more complex) type signature. For example, `Some[Int](1)` may be replaced by any function `Int => Some[Int]`. 
Ones are used only for samples simplicity.

The following looks intuitively clear:
1) If we have the value or expression of `Monad[T]` type, and we access on it the property or method that belongs 
   to the inner type `T` with result type `R` then the result should (! intuition :smile: ) have `Monad[R]` type 
   (if the `Monad` type does not have the same property or method - this moment is the property of current implementation, 
   but generally controversial):
```scala
  Some(1) + 2 // = Some(3)
  Option.Empty[Int] + 3 // = Option.Empty[Int]

  Some("one").length // = Some(3)
  Option.Empty[String].length // = Option.Empty[Int]

  List(-2.0, 2.0).sign // = List(-1.0, 1.0)

  Try(1) + Some(2) // = Try(Some(3)) or Some(Try(3))
  Failure[Int](exception) + Some(2) // = Failure[Option[Int]](exception) or Some(Failure[Int](exception))
  Try(1) + Option.Empty[Int] // = Try[Option[Int]](None) or Option.Empty[Try[Int]]
```
2) When we pass the value or expression of type `Monad[T]` as a parameter to the function with result type `R` where 
   type `T` is expected then result should have `Monad[R]` type.

   When the function with result type `R` and the type `Monad` have common monadic supertype then the result may be flattened:
```scala
  def getUser(userId: String): User
  getUser(Some("uid")) // : Option[User] = Some(user)
  getUser(Option.Empty[String]) // : Option[User] = None

  def findUser(userId: String): Option[User]
  findUser(Success("uid")) // : Try[Option[User]] = Success(Some(user))
  findUser(Failure[User](exception)) // : Try[Option[User]] = Failure[Option[User]](exception)
  findUser(Some("uid")) // flattened : Option[User] = Some(user) or : Option[Option[User]] = Some(Some(user)) 
  findUser(Option.Empty[String]) // flattened : Option[User] = Option.empty[User] or : Option[Option[User]] = Option.Empty[Option[User]]

  def add(term1: Int, term2: Int): Int
  add(Success(1), Some(2)) // : Try[Option[Int]] = Try(Some(3)) or : Option[Try[Int]] = Some(Try(3))
  add(Failure[Int](exception), Some(2)) // : Try[Option[Int]] = Failure[Option[Int]](exception) or : Option[Try[Int]] = Some(Failure[Int](exception))
  add(Success(1), Option.Empty[Int]) // : Try[Option[Int]] = Try[Option[Int]](None) or : Option[Try[Int]] = Option.Empty[Try[Int]]
```
Ok, looks like something is missing to be a bugs free Scala code? :smile:

So, to implement our intuition expectation something should satisfy typechecker and add the missing obvious manipulation 
code to construct the result of `Monad[R]` type. That is done by the set of implicits and `transparentMonads` macro. 
To build the correct code macro "recovers" the stack of for-comprehensions from the passed typechecked code where 
native Scala syntax bugs are replaced by compile time implicit calls.

Since there are variations of stacked result type like in the last examples, macro takes exactly one type parameter - 
the type of the built result (its nested monadic types init is `for`s stack types).

### Satisfying typechecker
The set of bundled implicits consists of general Transparent Types set, Transparent Types companions of 
Scala Predef implicits and set of helper ones. All implicits are currently defined for max stack depth of 5.

Each implicit name starts with `tts` prefix (Transparent TypeS) followed by its depth number (currently 1 to 5).
*Transparent TypeS*: "types" because ones are still not a monads at this level and any one-parameter type may be caught by these implicits.
The general `ttsN` implicits set is the following:
```scala
@compileTimeOnly(ctom) implicit def tts1[T1[_] <: AnyRef, T](t: T1[T]): T = ???
@compileTimeOnly(ctom) implicit def tts2[T1[_] <: AnyRef, T2[_] <: AnyRef, T](t: T1[T2[T]]): T = ???
@compileTimeOnly(ctom) implicit def tts3[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T](t: T1[T2[T3[T]]]): T = ???
@compileTimeOnly(ctom) implicit def tts4[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T](t: T1[T2[T3[T4[T]]]]): T = ???
@compileTimeOnly(ctom) implicit def tts5[T1[_] <: AnyRef, T2[_] <: AnyRef, T3[_] <: AnyRef, T4[_] <: AnyRef, T5[_] <: AnyRef, T](t: T1[T2[T3[T4[T5[T]]]]]): T = ???
```
`AnyRef` upper bound is required to make the implicits priority order in this set. `tt5` has higher priority and is applied first while resolving implicits overloads,
thus if stack types have the same method overload then this method is applied to the value of the most inner type of the stack which has such overload:
```scala
  Some(Seq("1", "12")).length // : Option[Seq[Int]] = Some(Seq(1, 2)) but not Some(2)! since in Option[Seq[String]] both Seq & String have method .length but String is more inner type
  tts2[Some, Seq, String](Some(Seq("1", "12"))).length // the same but typechecked with implicits
```
When we need to access the inner value with non-default depth, `ttsN` implicits may be called explicitly skipping types: 
ones will be derived by typechecker.
For above example, to get the length of inner `Seq` (not the `Seq` of lengths of each inner string):
```scala
  tts1(Some(Seq("1", "12"))).length // : Option[Int] = Some(2)  
```

Other bundled `ttsNxxx` implicits make support for Scala features realised by the Predef, primitives conversion implicits 
and additionally includes some helper `identity` ones.

Implicits applied to our samples make typechecker happy now:
```scala
  tts1intIdentity[Some, Int](scala.Some.apply[Int](1)).+(2) // = Some(3)
  tts1stringIdentity[Some, String](scala.Some.apply[String]("one")).length() // = Some(3)

  tts1doubleWrapper[List, Double](List.apply[Double](-2.0, 2.0)).sign // List(-1.0, 1.0)

  tts1intIdentity[Try, Int](Try.apply[Int](1)).+(tts1intIdentity[Some, Int](scala.Some.apply[Int](2))) // = Try(Some(3)) or Some(Try(3))

  def getUser(userId: String): User
  getUser(tts1stringIdentity[Some, String](Some("uid"))) // : Option[User] = Some(user)
  
  def findUser(userId: String): Option[User]
  findUser(tts1stringIdentity[Try, String](Try("uid"))) // : Try[Option[User]] = Try(Some(user))
  
  def add(term1: Int, term2: Int): Int
  add(tts1intIdentity[Try, Int](Try(1)), tts1intIdentity[Some, Int](Some(2))) // : Try[Option[Int]] = Try(Some(3)) or : Option[Try[Int]] = Some(Try(3))
```

Applying of the general `ttsN` or other `ttsNxxx` implicits drops outer types of the stack and leads to the expression type 
becomes one of its inner type after the typecheck. That makes possible syntactically valid applying of the required accessor of inner type. 

Applying of all required implicits to tms-values and tms-expressions makes the type of the entire input expression 
being the yield type of the built `for`s stack.

So, implicits satisfy typechecker and designate the tms-values and tms-expressions inners access along with dropped types 
by wrapping them to non-implemented compile time method calls.   

### Transparent Monads type approximation
As noted above, the stack of *Transparent Monads* `M1[M2[M3[T]]]` may be approximated as `M1[M2[M3[T]]] with M2[M3[T]] with M3[T] with T`
giving higher priority to overloaded accessors that are of the most inner type `T`.

The implementation based on implicits breaks this approximation by cyclic types shifting.
Actually it is `M2[M3[T]] with M3[T] with T with M1[M2[M3[T]]]`. The reason is simple and clear:
implicits will not be applied at all to the initial value of the type `M1[M2[M3[T]]]` when we call on it the accessor that 
is present in this type `M1[M2[M3[T]]]`.

It is not clear is it good or bad for real coding practice but this deviation leads to breaking above simple approximation 
of Transparent Monads.

For example, accessing `.length` on the value of `List[String]` type we'll get the length of the `List` but not 
the "generally tms expected" `List` of lengths of containing strings.
When we do `.length` on `Try[List[String]]` value then it dives to `String` type value and returns `Try` of 
`List` of string lengths, not the length of the `List` inside `Try`.

But it is absolutely good for this realisation with mapping to the `for`s stack: `.flatMap` and `.map` methods of 
a monadic value should be accessed first and without implicits applying. Otherwise, if these methods are applied to 
the inner type values (to the inner monad) first then after macro emits the processed code and `for`s stack is desugared 
to `.flatMap`/`.map` calls the implicits want to be applied again! :smile:

It is also normal (and may be ignored in the first approximation) because we daily deal with data monads wrapped to the control flow monads.
Except monadic methods they usually do not intersect by data manipulation accessor names.

### Types Stack overloads resolving order
In the current implementation based on implicits, for the value of type `M1[M2[M3[T]]]` we have the set of accessors 
defined by the something like `M2[M3[T]] with M3[T] with T with M1[M2[M3[T]]]` type.
Suppose we call the `prop` on that value (the same is valid for `method(...)` call). The resolving order of 
what type of inner value will be accessed by the overloaded `prop` is:
1) if the most outer type `M1` has `prop` then it is applied first: no implicits are required and the native `prop` of `M1` always is accessed
2) if only one of 3 inner types (`M2`, `M1`, `T`) has `prop` then it is applied to value of that type
3) if several of 3 inner types (`M2`, `M1`, `T`) has `prop` then it is applied to a value of that type which is the most inner of all having `prop`

The only exception is that among typed collections the `Array[T]` has higher priority over others even if other type collection is inner than `Array` in the types stack. 
This happens due to the Scala `Array` accessors implementation is based on Predef implicit extensions.  

### Building embracing Fors Stack
To access the inner type `T` value method of the monadic value with type `T1[T2[T]]` we should twice apply `map` or "open" (extract) it inside two `for`s:
```scala
  val t1t2t: T1[T2[T]]
  for { // T1 type for
    t2t <- t1t2t 
  } yield {
    for { // T2 type for
      t <- t2t
    } yield {
      t.method
    }
  }
```
That is a simple explanation of what `transparentMonads` macro does: it replaces each `ttsNxxx[T2, T1, T](t: T1[T2[T]]): T` 
call by the inner value of `t` obtained in the built embracing `for`s stack.

Initial empty `for`s stack builder consists of stack of `for`s for every one-parameter type of the input type `T1[T2[T]]` 
(in general: of the type parameter `O` of the macro call `transparentMonads[O]`.

There are 2 `for`s for `T1` and `T2` types in the built `for`s stack in our example. Each `for` "opens" (extracts) 
monadic values in the order of the input expression evaluation (for its type). So each `ttsNxxx` call is replaced by "opened" (extracted) 
inner value of the monad that is got in `for`s stack by adding corresponding number of `for`-enums required to get the value 
of desert type `T`. Types of `for`s to add to are the init (all except `T`) of sequence of type parameters 
in the `ttsNxxx[T2, T1, T](t: T1[T2[T]]): T` call that is being replaced.

For example (`tms` is the short name equivalent of `transparentMonads` macro):
```scala
  Try(1) + Some(2) + Try(Some(3)) // intuition: may result only one type: Try[Option[Int]] = Try(Some(6)) since Try in Try(Some(3)) should be "opened" first

  // typechecked with ttsN implicits and dropped packages for clarity:
  tts1intIdentity[Try, Int](Try.apply[Int](1))
    .+(tts1intIdentity[Some, Int](apply[Int](2)))
    .+(tts2intIdentity[Try, Some, Int](Try.apply[Some[Int]](Some.apply[Int](3))))

  tms[Try[Option[Int]]] { // the actual usage
    Try(1) + Some(2) + Try(Some(3))
  }

  // [debug] * tms extracted fors code view:
  { // macro output code in fors-view (before the final typecheck and desugar)
    for {
      valueOfTry$macro$1 <- Try.apply[Int](1)
      valueOfTry$macro$3 <- Try.apply[Some[Int]](scala.Some.apply[Int](3))
    } yield {
      for {
        valueOfSome$macro$2 <- scala.Some.apply[Int](2)
        valueOfSome$macro$4 <- valueOfTry$macro$3
      } yield {
        valueOfTry$macro$1.$plus(valueOfSome$macro$2).$plus(valueOfSome$macro$4)
      }
    }
  }
```

Scala Predef and primitives conversion `ttsNxxx` implicits magic is: after replacing `ttsNxxx` wrapper by 
the extracted inner value of monadic expressions the native Scala implicits are applied to that value 
(during the final typecheck and before the resulting macro code is returned).

### Local definitions limitation {#tms}
Macros may accept any Scala code that is available for the block of statements including definitions. 
`trnsparentMonads` is good for a solid even complex expressions. 

Just we define a local value and try to use it as tms-value (Transparent MonadS value) we fall into definition scope limitation.

Macros always output a block of code thus any definitions are local for this block.
If we define a local method or value then it may not be used inside any `ttsNxxx` call (may not be or be inside tms-expression). 
The reason is simple: to evaluate expression wrapped to `ttsNxxx` call (and replace it by extracted value) we should place it into embracing `for`s stack 
but that is apriori out of local definitions scope. Macro checks attempted use of local definitions inside `ttsNxxx` calls and emits the corresponding error.   

The following Monadic Flow Control is free of this limitation for stable val definitions of the root block of passed code.


## Monadic Flow Control

### A couple of abstract reasoning
What is between statements? Context, exceptions may to fly up to the root call, implicits light from top or from the side. 
Libraries and JDK - under the code, execution threads and JVM are somewhere in the depths and later...

We now wrap exceptions to the monad and let them live in the monadic "error-pipe", the data flows and is transformed in the monadic "data-pipe".
Monadic "error-pipe" bypasses data manipulations until recovered. Two that pipes intersect by the special methods to process errors 
and/or redirect monadic flow from the "error-pipe" to the "data-pipe".
So exceptions or errors now flow "down" on the same path as the data manipulation flows bypassing knots of data transformation.

Two sequential statements are still wired by the oldest assembler-times rule: 
when the first one is finished without exceptions then the second one is started (interruptions do not count at this level of abstraction :smile: ).  
Nothing happens between two declared statements at the code layer and nothing controls this sequence. 

Taking a look to the two sequential arrow enumerations of the for-comprehension header (two `<-` lines that looks like the statements) 
we'll see that there is the data-controlled second arrow-statement (including the rest part of the `for`) "execution". 
It will conditionally "start execution" (actually: it will call the function with the rest of the `for` definition and previous statement's data) 
not only when no exceptions occur but also only when the monad of the previous arrow-statement contains (next!) data: not an Exception for Future/Try, 
when Option is not a None or when the last element (if exists) of the first statement collection is not "executed", etc.    

Along with Transparent Monads syntax the Monadic Flow Control interpretation treats the regula code as a sequence of statements that 
are joined by above data-controlled rule of the next statement execution. 

### Intuition {#mfc}
Please, take a look to the following semi-virtual but working example:
```scala
  case class User(id: String)
  case class UserBalance(amount: BigDecimal, currencyId: String)
  case class BalanceInfoRequest(currencyId: String)
  case class BalanceInfoResponse(currencyId: String, currencyBalance: BigDecimal)

  def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest) = {
    val user = if (sessionIsActive(sessionId)) getUser(sessionId) else raiseError(new Exception("Bad session"))
    val userBalance = getUserBalance(user.id)
    val currencyBalance = if (userBalance.currencyId == request.currencyId)
      userBalance.amount
    else
      userBalance.amount * getExchangeRate(userBalance.currencyId, request.currencyId)

    val response = BalanceInfoResponse(request.currencyId, currencyBalance)

    saveAudit(sessionId, request, response)

    response
  }
```
Mmm... Looks a little familiar, but ugly a bit (virtual :smile: ), we already have User in the controller by session, but possibly should work... 

After a while, the question arises: but what are the types of methods ?!
It is ok assuming return type is exactly `BalanceInfoResponse` and other methods have plane non-monadic result types:
```scala
  def sessionIsActive(sessionId: String): Boolean
  def getUser(sessionId: String): User
  def getUserBalance(userId: String): UserBalance
  def getExchangeRate(fromCurrencyId: String, toCurrencyId: String): BigDecimal
  def saveAudit(sessionId: String, request: BalanceInfoRequest, response: BalanceInfoResponse): Unit
  def raiseError(exception: Exception): Nothing
```
Code is simple and easy readable (as any other *sync* or monads free code :smile: ).

To use monadic results of above methods we should put its statements to for-comprehension or add the set of wiring `flatMap`s. Something like the following with `for`:
```scala
  def sessionIsActive(sessionId: String): Try[Boolean]
  def getUser(sessionId: String): Try[User]
  def getUserBalance(userId: String): Try[UserBalance]
  def getExchangeRate(fromCurrencyId: String, toCurrencyId: String): Try[BigDecimal]
  def saveAudit(sessionId: String, request: BalanceInfoRequest, response: BalanceInfoResponse): Try[Unit]
  def raiseError(exception: Exception): Failure[Nothing]

  def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest): Try[BalanceInfoResponse] =
    for {
      isSessionActive <- sessionIsActive(sessionId)
      user <- if (isSessionActive) getUser(sessionId) else raiseError(new Exception("Bad session")) 
      userBalance <- getUserBalance(user.id)
      currencyBalance <- if (userBalance.currencyId == request.currencyId)
        Try(userBalance.amount)
      else
        getExchangeRate(userBalance.currencyId, request.currencyId).map(_ * userBalance.amount)
      response = BalanceInfoResponse(request.currencyId, currencyBalance)
      _ <- saveAudit(sessionId, request, response)
    } yield {
      response
    }
}
```
Looks more complex. So, the aim of `monadicFlowControl` macro is to keep the code manipulation simplicity as much as possible (ideally: like non-monadic one). 

The macro treats the passed block of the statements as having the "Monadic Flow Control" and recovers the for-comprehension:
```scala
  def getBalanceInfoResponse(sessionId: String, request: BalanceInfoRequest): Try[BalanceInfoResponse] =
    monadicFlowControl[Try[BalanceInfoResponse]] {
      val user = if (sessionIsActive(sessionId)) getUser(sessionId) else raiseError(new Exception("Bad session"))
      val userBalance = getUserBalance(user.id)
      val currencyBalance = if (userBalance.currencyId == request.currencyId)
        userBalance.amount
      else
        userBalance.amount * getExchangeRate(userBalance.currencyId, request.currencyId)

      val response = BalanceInfoResponse(request.currencyId, currencyBalance)

      saveAudit(sessionId, request, response)

      response
    }
```
Typechecked code at the macro input (with dropped packages) will look like the following:
```scala
//[debug] * tms INPUT code.tree: 
{
  val user = if (tts1[Try, Boolean](sessionIsActive(sessionId)))
    getUser(sessionId)
  else
    raiseError(new Exception("Bad session"));
  val userBalance = getUserBalance(tts1[Try, User](user).id);
  val currencyBalance = if (tts1[Try, UserBalance](userBalance).currencyId.==(request.currencyId))
    tts1[Try, UserBalance](userBalance).amount
  else
    tts1[Try, UserBalance](userBalance).amount.*(tts1[Try, BigDecimal](getExchangeRate(tts1[Try, UserBalance](userBalance).currencyId, request.currencyId)));
  val response = BalanceInfoResponse.apply(request.currencyId, currencyBalance);
  saveAudit(sessionId, request, response);
  response
}
```
Extracted macro output (with dropped packages) in `for`s code view:
```scala
//[debug] * tms extracted fors code view:
{
  for {
    valueOfTry$macro$1 <- sessionIsActive(sessionId)
    user = if (valueOfTry$macro$1)
      getUser(sessionId)
    else
      raiseError(new Exception("Bad session"))
    userValue$macro$2 <- user
    userBalance = getUserBalance(userValue$macro$2.id)
    userBalanceValue$macro$3 <- userBalance
    valueOfTry$macro$4 <- getExchangeRate(userBalanceValue$macro$3.currencyId, request.currencyId)
    currencyBalance = if (userBalanceValue$macro$3.currencyId.$eq$eq(request.currencyId))
      userBalanceValue$macro$3.amount
    else
      userBalanceValue$macro$3.amount.$times(valueOfTry$macro$4)
    response = BalanceInfoResponse.apply(request.currencyId, currencyBalance)
    wcMf$macro$5 <- saveAudit(sessionId, request, response)
  } yield {
    response
  }
}
```
Monadic values of vals like `user` and `userBalance` may also be used directly for `transform` or `recover` calls on ones. 
If ones are not used then such assign enum for-vals are optimized. `getExchangeRate` is called even if requested currency matches balance currency.
Side effects require separate attention.

Optimised (postprocessed) output code is:
```scala
//[debug] * tms postprocessed fors code view:
{
  for {
    valueOfTry$macro$1 <- sessionIsActive(sessionId)
    userValue$macro$2 <- if (valueOfTry$macro$1)
      getUser(sessionId)
    else
      raiseError(new Exception("Bad session"))
    userBalanceValue$macro$3 <- getUserBalance(userValue$macro$2.id)
    valueOfTry$macro$4 <- getExchangeRate(userBalanceValue$macro$3.currencyId, request.currencyId)
    currencyBalance = if (userBalanceValue$macro$3.currencyId.$eq$eq(request.currencyId))
      userBalanceValue$macro$3.amount
    else
      userBalanceValue$macro$3.amount.$times(valueOfTry$macro$4)
    response = BalanceInfoResponse.apply(request.currencyId, currencyBalance)
    wcMf$macro$5 <- saveAudit(sessionId, request, response)
  } yield {
    response
  }
}
```
Unlike `monadicFlowControl` the `transparentMonads` macro with the same code passed will fail with error:
"locally defined (in tmsCode) 'value user' may not be used inside such expression since this expression should be used in
Fors Stack outside of 'value user' definition scope..."

The meaning (and a primary aim) of the macro and of the idea of Monadic Flow Control is: 
we change the methods result type to monadic (or change the monad), but the code remains absolutely unchanged and as simple as non-monadic at all. 
With the help of the macro the monadic code may be defined more simple and be more clear to read.

So it treats source block statements not just having unconditional statement-by-statement flow control, but having Monadic Flow Control, 
counting the monadic nature of statements of the auto-detected Monadic Flow Type.

`monadicFlowControl` macro:
- detects the Monadic Flow Type by the first statement (by first stable val definition or expression)
- separates tailing "yield" part (after the last: stable val definition of any type or expression of Monadic Flow Type) 
- groups non Monadic Flow Type "header" expressions to a blocks (which goes to the `for` `_ =` block)
- validates definition scopes usage
- converts the source block to the for-comprehensions stack (or one `for`) built on above parts: 
  stable val definitions goes to `<-` or `=` for-enums depending on one conforming to Monadic Flow Type.

The last expression of the input block may also be of Monadic Flow Type, and the result may be flattened (depending on the input types stack `O`).

Monadic Flow Control macro realisation is based on Transparent Monads macro. The detected Monadic Flow Type may be 
an any type of the passed types stack `O`. This means that Transparent Monads other than of Monadic Flow Type also may be used 
(or be of combined types), but the possibility to use ones depends on the usage place (initializers of Monadic Flow Type vals 
or Monadic Flow Type expressions, non Monadic Flow Type vals or expressions, yield) thus on the `for`s stack level  
where such tms-value should be extracted and on the stacked type of the tms-value.  

### Local definitions limitation {#mfc}
Unlike Transparent Monads, the Monadic Flow Control root block stable val definitions may be used inside `ttsNxxx` calls 
since ones now are the `for` values (`<-` or `=`) by the construction and are scoped to any code that follows the for-definition of that vals.

As a first approximation, Monadic Flow Control has another limitation of non-stable val definitions: *local* lazy val, var, type, 
class, object and method definitions may occur in the input code only after the last stable val definition (of any type) or 
expression of the Monadic Flow Type: only inside the resulting `yield` part. The reason for current implementation is that 
ones have no for-comprehension analogs like stable val definition has. 

In some cases this restriction may be solved by the manual denoting of the `yield` part of input code with embracing block 
which will include tailing stable vals of *non Monadic Flow Type* (which are not used in tms-expressions) and, possibly, 
preceding expressions. Moving such definition down to the end of the block also may help.  

Thus, *local* lazy val, var, type, class, object or method definition still may not be used in `ttsNxxx` calls for the same reason 
as for Transparent Monads: the try to evaluate expression containing such locally defined ident in the embracing `for`s stack 
results in an out-of-scope usage and is reported as an error.

Such definitions that are defined *outside* the macro (*non-local* for macro) still may be used as Transparent Monads 
(or be the part of tms-expressions) in the macro input code.  


## Conclusion
Thank you for your interest to the question raised.

Unfortunately the strong theory basis or similar works were not found. Now it is only due to coding practices and 
belief in the feasibility. Hope somebody will catch the theory so ideas may be better typed and systematized with 
a simple and clear minimal limitations.

The aim was to raise the questions and to give the initial working implementation of ideas to play with, to research and 
to feel that it is actually realizable but has constrains (hoping the most ones are surmountable). To do the next steps.
To probe limitations for further thinking, to feel it and to find a holistic core of these and new ideas.


## What to play with
Scala implementations of the Transparent Monads syntax and Monadic Flow Control interpretation ideas 
are at [tms and mfc home] (or in the root of repo or artifact).

Project's *readme.md* contains definition and explanation of the following macros options: 
- helper pre-evaluation values options (usable for the parallel `Future`s evaluation start before `for`) 
- options for several additional ways of `for`s stack reconstruction to control inner 
  tms-values evaluation order of subexpressions by building the additional inner `for`s stacks 
- Predef any2stringadd compliance option
- options to debug, trace and monitor `for`s stack reconstruction

It summarises all found limitations, briefly describes how macros work.

[tms and mfc home]: https://github.com/SerhiyShamshetdinov/sugar-tms