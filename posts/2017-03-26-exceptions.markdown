---
title: error-handling-and-exceptions
summary: In this episode we discuss how to communicate errors and how exceptions can be an anti pattern if not used carefully.
soundcloud: 314465945
author: Amir Barylko
---

### Exceptions should be exceptional

Error handling is a key part of any application. 

However, using exceptions instead of a return type to express a function call can fail can be a bit confusing.

For example let's take a look at `java.lang.Integer.parseInt`

``` java
public static int parseInt(String s) throws NumberFormatException
```

The signature of `parseInt` expresses clearly that it may fail by throwing a `NumberFormatException`.

So either we need to catch the exception and see if we can do something about it, or perhaps we could just let the exception bubble up to the next `catch`.

Being honest though, we do not want to let a `NumberFormatException` escape from our function because is meaningful to the function context but not outside.

We can not avoid dealing with the exception. Maybe we could return a new type that is a bit more helpful, like perhaps a `Maybe<int>` or `PossibleResult<int>` instance that would help us take a decision without involving `try` and `catch`.

Wait... why not `null`? Well, here's why: [Tony Hoare - The billion dollar mistake](http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare).

## The _Optional_ value

In Java 8 a new class was introduced to capture similar scenarios.

The [`Optional<T>`](https://docs.oracle.com/javase/8/docs/api/java/util/Optional.html) represents a value that may or may not be there. It has functions like `map` that help to transform the possible result if present. Another useful function is `orElse` to obtain the value or provide an alternative. 

With that in mind, `parseInt` could change to

``` java
public static Optional<Integer> parseInt(String s)
```

And we could use it to decide what to do in case it fails by chaining functions

``` java
public Integer getPortNumber(String strPort) {
  return parseInt(strPort).orElse(8080);
}
```

In _Clojure_ is not idiomatic to use an `Optional` class, nor a `Maybe` type like [Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses).

However, there is a value chosen to represent `Nothing` and that value is `nil`.

The difference is that the `and` and `or` functions know how to work with it.

``` clojure
(defn get-port-number 
  [str-port]
  (or (parse-int str-port) 8080))
```

## A guideline to consider

Use exceptions when an unexpected event happens that there is not coming back from it.

_Run out of memory_, _file not found_, _network unreachable_, etc.

Every function/method we write is establishing a contract with the user. If failure is a possible result then we should reflect that in the return type.

For example calling an HTTP API always can fail. In `Clojure` we can return a map with different keywords that represents the result.

When the call succeeds, then the `:body` keyword will be there, and when the call failed, an `:error` keyword will indicate what is the error.

Similar to Haskell's [Either](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Either.html) data type in Java we have to be a bit more creative and have a class that represents success or failure.


