---
title: performance-and-style
summary: In this episode we discuss the tradeoffs between performance and style.
soundcloud: 361868549
author: David Taylor
---

## Performance and Style

At time these philosophies seem to object with one another. Best case scenario: thee most performant code — is also thee most stylistic, simple and understandable. However, this is often not the case. What do we do when find ourselves in a situation where we have to choose one or the other? Often, I find myself reasoning about this philosophy and have decided to choose style, then waiting for the need to optimize to present itself. 

Is this the right answer? Arguably, it's not. Each designer must choose for herself the philosophies and systems that will guide their development. The way that I have convinced myself that style come first, was through thinking about the complete life cycle of a code base. By defaulting to style, throughout the lifecycle of my code, I will end up being right more often than I will be wrong — and the time it will take to fix when I’m wrong will be smaller than vice versa. In a world of percentages I’ve chose the system that is most often right. 

Good style enables quicker development for all actors. The side effects of this philosophy lead me to believe that the identification of bugs, proliferation of knowledge and ability to understand the context of the program in order to contribute will be better — if I choose style first. However, having this philosophy open one up to criticism from those who prefer performance — so you might as well prepare for those conversations. 

I need to fine the commit where a bug was introduced. I have a function — which is expensive — that tests the commit and need to find the exact commit the bug was introduced. The second example uses a binary search a — but is ugly and lacks style. 

A developer will always be choosing between a different set of trade offs; There are no absolute right answers. However, it would do the community well to raise the value of style when reasoning about performance. 

Let’s take the example from the podcast. Say we want to find the commit number a bug was introduced and the function `is-good` will tell us that the given version is bug free. Arbitrarily, I picked commit 10 to demonstrate. 

```
(def commits [0 1 2 3 4 5 6 7 8 9 10 11 12]) 

(defn is-good [n] (< n 10))
```

A clean and simple solution could be written as followed. 

```
(defn clean-get-commit [] (->> commits (take-while #(is-good %)) last inc))

```
Using a binary search we can optimize the solution to be faster, however, it is uglier and less clear what is going on. 

```
(defn binary-search-commit [f l]
  (let [n (int (Math/floor (/ (+ f l) 2)))]
    (if (<= (- l f) 1) l 
        (if (is-good n)
          (recur n l)
          (recur f n)))))

(defn dirty-get-commit [] (binary-search-commit (first commits) (last commits)))
```