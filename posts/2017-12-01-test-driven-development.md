---
title: test-driven-development
summary: In this episode we whether of not there is still enough value in test driven development.
soundcloud: 361855742
author: David Taylor
---

## Is Test Driven Development still worth it? 

Yes: But you need to be smart about it. There is no doubt the software development world views testing as important. I’m happy to say that in my circles many value the skills needed to write quality tests. We are starting to regard features and enhancements to tests, as valuable as production code. 

Tests are code. Writing more tests will also raise the likelihood of adding bugs, confusion, and mistakes into your code base — just like adding more production code. You should approach the introduction of test code with the same reverence as production code. You should look for opportunities to refactor, simplify, optimize and improve your tests when possible. Further, you should think often about what value a test brings to your code base. There are times when a test isn’t worth the time, risk or upkeep — despite it validating some part of your code.

Alternatively, there are other ways to develop code that many programmers find beneficial. For example, many developers use a REPL to quickly build functionality. This quick feedback loop enables a developer to write solutions faster than those starting with test can. However, much of the benefit from automated testing comes long after the tests have passed and the feature is released into production. These tests continue to give a team value as they act as validation against regression issue — something the REPL will not help you with. 

Test Driven development is really valuable, however, moving away from unit testing and toward property testing will raise your ability to write concise — yet focused — tests. 

Thinking in terms of properties allows you to capture the domain of a scenario in a simple and descriptive way - improving the value of test-driven development. 

Looking at advent of code 2015 day 1 we can see that choosing a property of the program and then testing the outcome of that code — given that property — has enabled the testing of a broader spectrum of functionality with minimal code. Neat!

## The Code

For example:

[Day 1](http://adventofcode.com/2015/day/1) requires the evaluation of open parentheses with a given input. 

How we think about tests influences the code that we will write. 

If I think about the problem in units, I may write a test to give me the result of a smaller function, thus directing me towards writing functions that work well with unit tests. However, a unit focused approach could lead to unnecessary testing with little to no benefits — that may not  do a great job describing the program's utility.

To demonstrate let's use Clojure's `test.check` and create a property test. 

Keep the following words of wisdom in mind:

1. ["property based testing requires you to reason about how your program should behave"](http://hypothesis.works/articles/what-is-property-based-testing/)

2. ["Rather than asserting that specific inputs to your code should result in a specific output, as with unit tests, property-based tests make statements about the expected behaviour of the code that should hold true for the entire domain of possible inputs. These statements are then verified for many different (pseudo)randomly generated inputs."](https://jonathangraham.github.io/2016/01/07/property_based_testing_clojure_functions)


First let's reason about how our program should behave. Our problem requires that given a sequence of parenthesis — which represent floors - that we determine the level we end up at. 

In this case our domain is a set of parenthesis. Let's create a generator that will give us parethesis. 

``` clojure
(require '[clojure.test.check.generators :as gen]) 
(require '[clojure.test.check :as tc])
(require '[clojure.test.check.properties :as prop])

(gen/sample (gen/elements "()"))
;; (\( \( \( \) \) \) \) \( \) \))

```

Great! Now we're able to write a test that describes the high-level specification of behavior for a range of data points. In this case, we need to implement logic which should "hold true for the entire docmain of possible inputs."

Below you'll notice that I have written a test that generates input — a random sequence of parenthesis — and then calculates the count I'd expect after running my funtion `floor`.

``` clojure
;; The code I'm testing
(def is-open [c] (if (= c \( ) 1 -1)))

(floor [s] (reduce (fn [acc v] (+ (is-open v) acc )) 0 s))

;; The property test
(def test-with-generators·
  (testing "generators"
   (prop/for-all [input (gen/vector (gen/elements "()"))]
     (let [floor-commands (apply str input)
           command-count (count floor-commands)
           floor-ups (count (filter #(= % \() floor-commands))
           expected (- floor-ups (- command-count floor-ups))]
            (is (=  expected (floor strs)))))))]))))

(tc/quick-check 100 test-with-generators)
```

Given that the property test will run 100 times each with a different set of inputs — these tests are more concise, easier to maintain, and paint a better picture of what the program does than a unit test covering the same domain.  

Test Driven Development is still worth it. You should use tests to drive the design of your programs. However, property testing will guide a developer towards squeezing the most value out of concise and clean tests. 
