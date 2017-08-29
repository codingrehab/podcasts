---
title: immutability-and-loops
summary: In this episode we discuss working with immutable data structures and how to replace common constructs from OOP that usually use loops with well known functions.
soundcloud: 333520893
author: Amir Barylko
---

## Immutability and loops

Working with immutable data structures can be a challenge when transitioning from languages like `Java`, `Javascript` or `C#` (where _having a state_ or _modifying the state_ is a common practice and idiomatic to the language) to languages like `Clojure`, `Haskell`, `F#`, `Elixir`, etc.

But what does it mean to be _immutable_? It means the all the code you write or use can not modify an instance of an object or data structure, rather it will create a new object that reflects the change you expected.

For example in `Clojure` to add a new element to a collection you could use `conj` 

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(conj [1 2 3 4] 5)

</code></pre>

The `conj` operation does not modify the collection but returns a copy of it instead

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(let [original [1 2 3 4] copy (conj original 5)]
  (str "The original " original " -- The copy " copy))

</code></pre>

Collections in languages that enforce immutability are optimized for these kind of operations so they are very efficient and there is no penalty for immutability.

## Friendly recursion

Immutability forces us to rethink how to write loops. Iterating over a collection or creating collections in a loop has to be relearned.

Lets consider transforming the elements of a collection one at a time by applying a function to each element and returning a new collection with the results.

Though we will not use a classic `while` loop, we can do something very similar by using _recursion_.

Here we see an example of _incrementing_ the elements of a collection one at a time:

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(loop [[x & xs] [1 2 3 4] result []]
  (if (nil? x)
    result
    (recur xs (conj result (inc x)))))
</code></pre>

Actually this operation is so common that there is a `for` function that does most of the work for us. This is usually called _list comprehension_ in many languages.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(for [x [1 2 3 4]] (inc x))

</code></pre>

Filtering elements given a predicate is also straightforward. Here is an example filtering even elements:

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(for [x [1 2 3 4] :when (even? x)] x)

</code></pre>

Here is another example for list comprehension taking the first ten elements:

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(for [x (range 1 20) :while (< x 11)] x)

</code></pre>

## Avoid custom loops

List comprehension help us to avoid repeating common operations.

Many of these concepts are indeed so common that specific functions are included in each language, to achieve the same functionality.

Transforming elements using a function is called _mapping_. Thus the function is called `map`.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(map inc (range 1 20))

</code></pre>

This idiom is so popular that many languages implement it. It is very similar in `Haskell` and `F#`

``` haskell
-- Haskell
map (+ 1) [1..20]
```

``` fsharp
// F#
List.map ((+) 1) [1; 2; 3; 4]
```
and even you can find it in `Java` and `C#` (though is called `Select`)

``` Java
// Java Part of `Stream` API
Arrays.asList(1, 2, 3, 4, 5).stream().map(i -> i + 1);
```

``` C#
// C# Using LINQ extension methods
new List<int>() { 1, 2, 3, 4, 5 }.Select(i => i + 1);
```

### Why bother?

You may we wondering 

> Why use other functions instead of just _list comprehensions_ or even a custom _loop_? 

Because functions like `map` have well defined clear meaning.

_Custom loops_ force the reader to invest effort into looking at the code, writing tests, etc to understand what was the intention behind the loop.

On the other hand, when using well known functions we are applying clearly defined building blocks that we do not need to explain nor document. All that effort comes as a boon and makes our code more idiomatic.

## Combining blocks

Using common building blocks make code more expressive and much easier to understand.

The more familiar we are with these blocks the easier is for us to combine them to satisfy our needs.

Here is an example of finding the first _even_ element in a collection: 

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(first (filter even? [5 3 19 21 6 18 3 4]))
</code></pre>

More idiomatic to `Clojure` here is the same code using the [thread last macro](https://clojuredocs.org/clojure.core/-%3E%3E).

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(->> [5 3 19 21 6 18 3 4]
     (filter even?)
     first)
</code></pre>

Here is another example taking all the numbers that are _greater than zero_, _incrementing_ them by one and filtering the _even_ ones. 

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(->> [5 3 18 21 -1 18 3 4]
     (take-while #(> % 0))
     (map inc)
     (filter even?))
</code></pre>

Though there is a combination of functions, each element is only traversed once and on demand — due to the lazy nature of sequences in `Clojure`.

## Food for thought 

I can not emphasize enough how important is to learn — as much as we can — which common concepts are already implemented and how to use those functions in our code.

Here is a small list as a starting point.

### Concatenation and mapping

We already talked about two of the most used functions `filter` and `map`. Mapping to a collection and then concatenate the result is a very common scenario. So common, that there is a function that composes both.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]])
</code></pre>

### Take and drop

Another common scenario is taking (and dropping) elements.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(take 10 (range))
</code></pre>

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(take-while #(< % 10) (range))
</code></pre>

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(drop 10 (range 1 20))
</code></pre>

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(drop-while #(< % 10) (range 1 20))
</code></pre>

### Zipping collections

So far we looked at working with one element at a time. Here is a very useful function to combine elements of two or more collections.

In `Haskell` the function is called `zip`, which takes two collections and returns a single collection of tuples. Each tuple containing one element of each collection.

``` Haskell
zip [1, 2, 3] [4, 5, 6] -- Returns [(1, 4), (2, 5), (3, 6)]

zipWith (+) [1, 2, 3] [4, 5, 6] -- Returns [5, 7, 9]

unzip [(1, 4), (2, 5), (3, 6)] -- Returns ([1,2,3],[4,5,6])

-- For more than two collections there is zip3, zip4 etc...

zip3 [1, 2, 3] [4, 5, 6] [7, 8, 9] -- return triplets [(1, 4, 7), ....]
```

Luckily for us (being a dynamic language) in `Clojure` to implement the same functionality we use the same `map` function that we saw in the first example.

The `map` function expects another function whose arity matches the collections passed as parameter. For two collections it should be a function that receives two parameters; Three collections a function that receives three parameters, and so on.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(map vector [1 2 3] [4 5 6] [7 8 9])
</code></pre>

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(map + [1 2 3] [4 5 6])
</code></pre>

### Reducing (folding)

Reducing a collection means, to transform all the elements into a value. This transformation does not happen one element at a time, but in conjunction with an accumulator — which can help us decide what to do.

The `reduce` function expects a function with two parameters (the accumulator and the current element), then an initial value and finally the collection from which to take the elements.

For example suming all the elements of the collection

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(reduce + 0 [1 2 3 4 5 6])
</code></pre>

Here is another example converting a collection into a _hashmap_

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(reduce (fn [h [k v]] (assoc h k v)) {} [[:a 1] [:b 2] [:c 3]])
</code></pre>

The intermediate steps can be obtained using `reductions`

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(reductions (fn [h [k v]] (assoc h k v)) {} [[:a 1] [:b 2] [:c 3]])
</code></pre>

`group-by` is an especial reduction that returns a `hashmap` using the function to determine the key of each element.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(group-by even? (range 1 20))
</code></pre>

`frequencies` is a reduction that checks the frequency of each element, and returns a `hashmap`. The hashmap contains the element as key and the count as a value.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(frequencies [2 2 1 4 4 1 4 2 3])
</code></pre>

### Extra icing

Here are two more functions that can be useful to have in your toolbox.

`partition` takes a number, a collection and returns a new collection, partitioned by the given amount of elements.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(partition 2 (range 20))
</code></pre>

And last but not least, here is a very useful function that mimics a scenarios where the same function is applied again and again over the result of the previous call.

And the sequence returned is `x, (f x), (f (f x)), (f (f (f x)))` and so on.

Because this produces an infinite sequence, we are explicitly taking the first twenty memebers of the sequence.

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(take 20 (iterate inc 0))
</code></pre>

<pre class="klipse"><code class="eval-clojure"  data-loop-msec="2000">
(def powers-of-two (iterate (partial * 2) 1))

(take 10 powers-of-two)
</code></pre>

## Summary

Writing custom loops is a lot of effort, and more often than not, there is an alternative already written to help you out. Functions often need to be combined with other functions and frequently require small helper functions.

Though list comprehensions, and many of the functions we just reviewed are very common in functional languages, they also exists in many others language.

Do not get discouraged, search for them, you may be surprised of what you will find.



