---
title: test-driven-development
summary: In this episode we whether of not there is still enough value in test driven development.
soundcloud: 361855742
author: David Taylor
---

## Is Test Driven Development still worth it? 

Yes: But you need to be smart about it. It seems that the software development world has converged upon the idea that testing is important. I’m happy to say that in my circles many value highly the developers who understand and push quality in testing. We are starting to hold features and enhancements to a testing infrastructure — along with test — to the same standards as production code. 

Tests are code. More tests — like code — raise the likelihood of introducing bugs, confusion and mistakes into your code base — just like production code. You should approach the introduction of test code with the same reverence as production code. You should look for opportunities to refactor, simplify, optimize and improve your tests when possible. Further, you should think often on what value a test brings to your code base. There are times when a test isn’t worth the time, risk or upkeep — despite it validating some part of your code. Beautiful test solution are important. 

This seems to come easier with functional programmers due to functional solutions focusing on the lifecycle of a problem — opposed to units a problem. Test Driven development is really valuable, however, moving away from unit testing and toward property testing will raise your ability to write minimal — yet optimal — tests. 

There is a lot of gray area here and to provide better contrast I’ll use extreme examples to illustrate the point  — while admitting that better unit tests could achieve the same purpose. However, I believe thinking in terms of properties allows you to achieve near optimal behavior more easily — improving the value of test driven development. 

Looking at one of Amir’s many advent of code submissions we can see that choosing a property of the program and testing the outcome of code — given that property — has enabled the testing of many units — with minimal code. Neat!


Test Driven Development is still worth it. You should use tests to drive the design of your programs. Property testing will guide towards squeezing the most value out of concise and clean tests. 

For example: 

```
(deftest parens 
  (is (= 1 (parens “(“)))
  (is (= -1 (parens “)”))))
```
A unit focused approach could lead to unnecessary testing with little to no benefits — that doesn’t do a great job describing the programs utility.

```
(deftest 
  (is (= [1 -1 1] (map-paren “()(“))))

(deftest ends-in-floor-3  
  (is (= 3 (visit "(((")))
  (is (= 3 (visit "(()(()("))))
```  