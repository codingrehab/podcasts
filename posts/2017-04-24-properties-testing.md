---
title: generative-testing
summary: In this episode we talk about using generative testing to easily create input values for our tests based on properties instead of hardcoding values
soundcloud: 319512031
author: Amir Barylko
---

## How many testing scenarios is enough?

Identifying scenarios when working on a feature help us write tests to ensure the feature works as expected. Once the test for each scenario passes then the whole feature is implemented correctly.

Each scenario brings a _domain_ of possible input values that make the test behave in a certain way.

For example, lets consider the function `positive?` that returns `true` if the parameter passed is greater than zero and `false` otherwise.

```clojure
(defn positive? [n] .... )
```

The __domain__ in this case are all the numbers. There is two clear scenarios to test:

1. The parameter is __greater than zero__, in which case it should return `true`.
2. The parameter is __equal or less than zero__, in which case it should return `false`.

How to decide which values we should use to test? What is the minimum value to use or the maximum value to use? Hard to tell...

We can come up with a few numbers... but seems a bit fragile (and silly).

## Properties testing

Instead of choosing a few numbers to test let's use instead a function that will generate random values from the specific domain for each scenario.

To do that we are going to use the [test.check](https://github.com/clojure/test.check) library.

Inspired by [QuickCheck](https://hackage.haskell.org/package/QuickCheck) the `test.check` library has several functions called _generators_ that represent the _base_ creators of possible input values.

<pre class="klipse"><code class="eval-clojure" data-external-libs="https://raw.githubusercontent.com/clojure/test.check/master/src/main/clojure/clojure/test/check.cljc" data-loop-msec="2000">
(require '[clojure.test.check.generators :as gen])

;; Generates int values
(gen/sample gen/int)
</code></pre>

Having two very clear scenarios makes easy to identify two _properties_ to test:

1. For every number that is greater than zero, `positive?` should return `true`.
2. For every number that is less or equal than zero, `positive?` should return `false`.

With that in mind, let's create two generators.

For positive numbers:

<pre class="klipse"><code class="eval-clojure">
(def pos-num-gen (gen/such-that #(> % 0) gen/int))

(gen/sample pos-num-gen)
</code></pre>

For zero and negative numbers:

<pre class="klipse"><code class="eval-clojure">
(def neg-num-gen (gen/such-that #(<= % 0) gen/int))

(gen/sample neg-num-gen)
</code></pre>

And having the generators ready, we can write the properties and verify them.

<pre class="klipse"><code class="eval-clojure">
(require '[clojure.test.check :as tc])
(require '[clojure.test.check.properties :as prop :include-macros true])

(defn positive? [n] (> n 0))

(def all-numbers-greater-than-zero-are-positive
  (prop/for-all [n pos-num-gen] (positive? n)))

(tc/quick-check 100 all-numbers-greater-than-zero-are-positive)
</code></pre>

And the second property:

<pre class="klipse"><code class="eval-clojure">
(def all-numbers-less-or-equal-than-zero-are-not-positive
  (prop/for-all [n neg-num-gen] (not (positive? n))))

(tc/quick-check 100 all-numbers-less-or-equal-than-zero-are-not-positive)
</code></pre>

## What about the "real world"?

Thinking in terms of properties frees us from focusing on each test case and broaden our view towards describing what is the _precondition_ and _postcondition_ of the function.

But how can we use this technique for real world scenarios? No every function will be as easy to find properties as `positive?`.

No, of course not.

The __test.check__ library provides ways to combine the generators in order to create more complex ones.

We saw a hint by using the `such-that` function to filter the generated values by applying a predicate.

Another very useful function is `fmap` that transforms the generated values to match the domain we need.

To illustrate the point let's use [plumatic.schema](https://github.com/plumatic/schema) to build a bank account representation.

```clojure
(require '[schema.core :as s])

(def BankAccount
  {:first-name s/Str
   :last-name  s/Str
   :source     (s/Enum "South", "North")
   :balance    pos-int})

```

Using the [schema generators](https://github.com/plumatic/schema-generators) library let's create a basic generator, and also a generator that returns only accounts from the _South_.

``` clojure
(require '[schema-generators.generators :as sgen])

(def basic-account-gen (sgen/generator BankAccount))

(def south-balance-gen (fmap #(-> % (assoc :source "South")) basic-account-gen))

```

And thus we can test it:

```clojure
(def all-accounts-from-the-south-are-processed-with-interest
  (prop/for-all [account south-balance-gen] 
                (= some-value-with-interest-here (process-account account))))

(tc/quick-check 100 all-accounts-from-the-south-are-processed-with-interest)

```

## But random values may fail randomly

Actually not. That's a misconception.

We choose the input values carefully to ensure that every element genereted belongs to the domain. Is true that we do not care which values are selected, but they are all from the same domain (for example _all positive numbers_).

Why is so much better? Because the focus on the properties simplifies searching for scenarios and helps us to avoid relying too much in the implementation.

Having said that, finding properties to test is not always easy, and some times we can combine both approaches. Some scenarios are easier to test with generative testing and some using a case by case methodology.

Similar libraries can be found in other languages like Scala, F# and Elixir.

