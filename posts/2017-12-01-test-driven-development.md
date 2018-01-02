---
title: test-driven-development
summary: In this episode we whether of not there is still enough value in test driven development.
soundcloud: 361855742
author: David Taylor
---

## Is Test Driven Development still worth it? 

Yes: But you need to be smart about it. There is no doubt in the software development world that testing is important. I’m happy to say that in my circles many value highly the developers who understand and push quality in testing. We are starting to view features and enhancements to tests, as valuble as  production code. 

Tests are code. Writing more tests will also raise the likelihood of adding bugs, confusion and mistakes into your code base — just like adding more production code. You should approach the introduction of test code with the same reverence as production code. You should look for opportunities to refactor, simplify, optimize and improve your tests when possible. Further, you should think often on what value a test brings to your code base. There are times when a test isn’t worth the time, risk or upkeep — despite it validating some part of your code.

Alternatively, there are other ways to develop code that many programmers find beneficial. For example, many developers use a REPL to quickly build functionality. This quick feeback loop enables a developer to write solutions faster than those starting with test can. However, much of the benefit from automated testing comes long after the tests have passed and the feature is released into production. These test continue to give a team value as they act as validation against regression issue — somthing the REPL will not help you with. 

Test Driven development is really valuable, however, moving away from unit testing and toward property testing will raise your ability to write concise — yet focused — tests. 

There is a lot of gray area here and to provide better contrast I’ll use extreme examples to illustrate the point — while admitting that better unit tests could achieve the same purpose. However, I believe thinking in terms of properties allows you to capture the domain of a scenario in a simple and descriptive way - improving the value of test driven development. 

Looking at one of Amir’s many advent of code submissions we can see that choosing a property of the program and testing the outcome of code — given that property — has enabled the testing a broader spectrum of functionality  with minimal code. Neat!

## The Code

For example:

From the advent of code 2016 [day 1](http://adventofcode.com/2015/day/1) it asks to evaluate how many open parenthesis are in a given input. 

How we think about tests influences the code that we write. 

If I think about the problem in units, I may write a test to give me the result of a small function. However, a unit focused approach could lead to unnecessary testing with little to no benefits — that doesn’t do a great job describing the programs utility: like test below.

``` clojure
(deftest parens 
  (is (= 1 (parens “(“)))
  (is (= -1 (parens “)”))))



(deftest 
  (is (= [1 -1 1] (map-paren “()(“))))
```

Moving to property testing we can see that the test describe the high-level specification of behavior for a range of data points. These tests are often  more concise, easier to maintain, and paint a better picture of what the program does. 

``` clojure
(deftest ends-in-floor-3  
  (is (= 3 (visit "(((")))
  (is (= 3 (visit "(()(()("))))
```  

Test Driven Development is still worth it. You should use tests to drive the design of your programs. Property testing will guide towards squeezing the most value out of concise and clean tests. 
