---
title: design-first-development
summary: In this episode we talk about the difference in thought process behind design when using different languages.
soundcloud: 361867784
author: David Taylor
---

## Design First Development 

When learning different languages a programmer will change the way in which he thinks about the program. When developing with Haskell, he may find himself starting with the interface or abstraction when beginning to write a programs. Clojure has the similar effect, a developer will tend to think about the structures. The natural inclination is to think upon the interfaces then develop the code which transforms the data. A programmer will adjust his thinking to a — from the data’s perspective — bottom down approach. 

In true coding rehab fashion, I’ll highlight how this is a complete paradigm shift compared to object oriented programming. Java leads a developer to think on primitives first. She will develop methods and classes as a interface to the data. When part of a whole system, she may add a few additional literal java `interface` classes as a further abstraction. This approach conforms to a more bottom up approach. This is by no accident. These approaches give key insights into the order in which the languages authors prioritize design traits. 

Example 

A java developer my lay out his primitives in an empty class

```
public class divider{

  int four; 
  public divider(){
    four = 4;
  }

}

```
Then start to build functions around those primitives to encapsulate and give access to the rest of the program. 

```

public class divider{

  int four; 
  public divider(){
    four = 4;
  }

  static void getTwo(){
    return four/2;
  }

}

```
Later after considering how it will need to interact in a system — the interface may be developed. 

```
interface divideBy {
  int getTwo(); 
}

public class divider implements divideBy{

  int four; 
  public divider(){
    four = 4;
  }

  static void getTwo(){
    return four/2;
  }

}

```      

In a functional language the design or interface tends to come first. Looking at one of our [favorite books](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#algebraic-data-types) we’ll use the shape example. 

First we think upon our data type

```
Data shape = Circle Float Float Float
```
At this point all we have is the interface or specification. We haven’t specified how we tend to use it. Next the author decides to create functions — naturally starting with the functions type definition first. 

```
surface :: Shape -> Float
surface: (Circle _ _ r) = pi * r ^ 2
```

You’ll notice the very last part to be created — of this small program — is the implementation with primitive types. Strange isn’t it?! It’s almost as if the language wants you to think about the design first.