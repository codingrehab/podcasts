---
title: parsing-combinators
summary: In this episode we discuss working with parser combinators and how to use them for every day tasks.
soundcloud: 361856174
author: Amir Barylko
---

## Parser Combinators

Parsing is applying a function with the type `String -> Parser -> a` where `a` is the result that you expect.

Some parsers will return one character, or a word while others could return a complete _AST_ (abstract syntax tree).

### What is a combinator?

Combinators are functions that take two or more elements of a type and return a new element of the same type.

For example we are very familiar with the `+` combinator for numbers or `concat` for lists.

Following the same reasoning a _parser combinator_ is going to be a _function_ that takes two or more parsers and rturns another parser.

### Why parser combinators?

Combining _small_ building blocks to create _bigger_ building blocks is an approach that we are acustomed to and use naturally.

Parser combinators libraries provide functions that start small (parsing digits, letters, etc) and then combinators that help to model common scenarios like repetition, delimiters and using multiple parsers together.

## A Parser Combinator library

[Kern](https://github.com/blancas/kern) is a parser combinator library for Clojure inspired by [Parsec](https://hackage.haskell.org/package/parsec).

The best way to learn about _Kern_ is to start working with it. We are going to work on an example soon, but let us get our feet wet by reviewing some of the basic parsers and combinators.

### Running parsers

To get the result of a parser, the following functions are available:

```clojure
;; Get the result of parsing

(value letter "abc") ; \a

;; Run the parser and print the result

(run letter "abc") ; nil

;; prints \a
;; returns nil


;; Parse and return a hash with information

(parse letter "abc")

;; return a structure
#blancas.kern.core.PState
{:empty false
 :error nil
 :input (\b \c)
 :ok true
 :pos #blancas.kern.core.PPosition {:col 2 :line 1 :src ""}
 :user nil
 :value \a}
```

### Primitive parsers 

Primitive parsers are the basic components of the library. All the other parsers are built on top. 

```clojure 
;; Parsing a letter
(value letter "abc") ; \a

;; Parse a digit
(value digit "123") ; \1

;; Parse a specific word until a separator (space, tab, punctuaction, etc)
(value (word "hello") "hello there") ; "hello"

;; Parse a number
(value dec-num "12345") ; 12345

;; Parse space
(value space " abcd") ; \space

```

### Applying multiple parsers combinators

Is common to use more than one parser to parse complex inputs. Here are a series of combinators that helps us working with multiple parsers.

```clojure
;; Try one parser and if it fails return the other
(value (<|> digit letter) "1ab") ; \1

(value (<|> digit letter) "a1b") ; \a


;; Return the value of every parser

(value (<*> (sym \+) digit dot digit) "+1.2") ; [\+ \1 \. \2]


;; Return the value of the last parser

(value (>> (sym \+) digit dot digit) "+1.2") ; \2


;; Return the value of the first parser

(value (<< (sym \+) digit dot digit) "+1.2") ; \+


;; Parse and concatenate the result of each parsing

(value (<+> (optional (sym \-)) dec-num) "-1234") ; "-1234"


;; Apply a function to the result of the parsing

(value (<$> read-string 
            (<+> (optional (sym \-)) dec-num)) "-1234") ; -1234
```

### Repetition

Repeating a parser is one of the basic ways of combining parsers.

```clojure
;; Repeat zero or more times a parser

(value (many letter) "xyz123") ; [\x \y \z]

(value (many letter) "5xyz123") ; [] because zero occurences is fine

;; Repeat one or more times a parser
(value (many1 letter) "xyz123") ; [\x \y \z]

(value (many1 letter) "5xyz123") ; nil because expects at least one occurrence


;; Optionally run a parser

(value (<+> (optional (sym \-)) dec-num) "-45") ; \-


;; Run a parser separated by other parser

(value (sep-by (sym \&) (many1 letter)) "a&bb&ccc&ddd") ; [[\a] [\b \b] [\c \c \c] [\d \d \d]]

```

## Example

Now that we are all warmed up, let us put the parser combinators to work.

We are going to use [day 7 of advent of code 2016](https://adventofcode.com/2017/day/7) as an exmple of using a parser combinator to solve a problem.

For _Day 7_ we need to parse the input that has the form:

<pre>
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
</pre>

Each line uses the concept of _program name_ so I am going define a parser for it.

A _program name_ is a four letter word:

```clojure
(value (times 4 letter) "gyxo") ; [\g \y \x \o]
```

Good start, but we need a string, or even better a `keyword`. Using the `<$>` function we can convert the result of parsing (a list of characters) into a keyword.

``` clojure
(def prog-name-p (<$> #(->> % (apply str) keyword) (times 4 letter)))

(value prog-name-p "gyxo") ; :gyxo
```

After the program name comes the value in parenthesis. Luckily we have a handy function to parse between parenthesis.


``` clojure 
(value (parens dec-num) "(22)") ; 22
```

We need both values, the program name and the value in parenthesis:

``` clojure
(def prog-val-p (<*> prog-name-p (skip-ws (parens dec-num))))

(value prog-val-p "abcd (67)") ; [:abcd 67]
```

First case done, now we need to parse a program that has "dependencies".

Something like `abcd (88) -> ktlj, cntj, xhth`.

As we did before, let us split the task. We do have a parser for a program with value, so let us focus on a parser for the dependencies.

Here the dependencies are a list of comma separated _program name_. Once again, we can use one of the already defined parser combinators `sep-by`.

``` clojure
(def dep-list-p (sep-by comma prog-name-p))

(value dep-list-p "abcd, efgh, ijkl, mnop") ; [:abcd :efgh :ijkl :mnop]
```

To combine both we need a ` -> ` in the middle. We could use the primitive parser `token` that does exactly that.

Also, because we only care about the list and not the token, we can use the `>>` combinator to get the value of the last parser only.

``` clojure
(def assign-p (>> (skip-ws (token "->")) (skip-ws dep-list-p)))

(value assign-p " -> aaaa, bbbb, cccc") ; [:aaaa :bbbb :cccc]
```

Almost ready! Now we need to combine two scenarios. One where we have only the program name and value and the other where also there are dependencies.

To model that we could use the two parsers we already have, making the dependencies one _optional_.

``` clojure
(def line-p (<*> prog-val-p (optional assign-p)))

(value line-p "abcd (87)") ; [[:abcd 87] nil]

(value line-p "abcd (87) -> aaaa, bbbb, cccc") ; [[:abcd 87] [:aaaa :bbbb :cccc]]

```

Another way to define the combination of both parsers is to use `bind` in the form of `let` so each _name_ is bound to the result of each parser.

Instead of having the concatenations of two lists (and maybe `nil` as a second element) we could concatenate all the values together as part of the same list.

``` clojure
(def line-p
  (bind [prg prog-val-p
         deps (optional assign-p)]
         (return (concat prg (or deps [])))))

(value line-p "abcd (87)") ; (:abcd 87)

(value line-p "abcd (87) -> aaaa, bbbb, cccc") ; (:abcd 87 :aaaa :bbbb :cccc)

```

## Is it worth it?

Here is a summary of the solution:

``` clojure
(def prog-name-p (<$> #(->> % (apply str) keyword) (times 4 letter)))

(def prog-val-p (<*> prog-name-p (skip-ws (parens dec-num))))

(def dep-list-p (sep-by comma prog-name-p))

(def assign-p (>> (skip-ws (token "->")) (skip-ws dep-list-p)))

(def line-p
  (bind [prg prog-val-p
         deps (optional assign-p)]
         (return (concat prg (or deps [])))))

```

Clearly this is much more code than a regular expression but it has many benefits over a reg exp.

The meaning for the `line-p` parser and the other parsers is much more clear, easy to test and easy to maintain if somebody else has to change the code later.

Parser combinator libraries are a very useful tool and being familiar with the library can save you lots of coding and debugging time.

