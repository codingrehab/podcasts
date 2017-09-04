---
title: databases
summary: In this episode we talk about working with databases in a functional way.
soundcloud: 330991445
author: Dave Taylor
---


## Databases

Interactions with a database can be expressed clearly and succinctly in a functional paradigm. Developers coming to functional programing from an object oriented background, may feel like something is simply missing and search for an abstraction. 

The functional approach is to simply make a connection and start creating functions. A DSL combined with the expressive power of compound procedures — apply and piping — a programmer can express his interactions with the database. These expressions are often both readable and intuitive, to other developers. 


### The Object Oriented Approach

Looking at some alternative Java approaches (which have been greatly simplified), we can begin to see the scaffolding needed to maintain these interactions. A side effect of these object oriented abstractions is code bloat. This implementation detail introduces the tendency to have the code drive the design of the database — often when not appropriate. 

 
#### Hibernate: 

This [popular library](http://docs.jboss.org/hibernate/orm/5.2/quickstart/html_single/) requires a programmer to create a class with meta data, before querying. Keeping this logic in sync with your database migrations is tedious.


```
@Entity
@Table(name = "BOOK")
public class BOOK {

  @Id
  @Column(name = "id")
  private long id;

  @Column(name = "name")
  private String name;

  @Column(name = "author")
  private String author;
```

```
Query query = session.createQuery("from BOOK");
    List<Car> empList = query.list();
```

#### JDBI


The JDBI [library](http://jdbi.org/sql_object_api_queries/) was designed to map queries to an interface. Like Hibernate, a developer needs to create a custom class implementing ResultSetMapper, mapping each element in a database to a class. 

```
public interface Queries
{
  @SqlQuery("select name from BOOK where id = :id")
  String findName(@Bind("id") int id);

  @SqlQuery("select name from BOOK where id > :from and id < :to")
  List<String> findNamesBetween(@Bind("from") int from, @Bind("to") int to);

  @SqlQuery("select name from BOOK")
  Iterator<String> findAllNames();
}
```



### The Functional Approach

Looking at the Clojure approach with [HoneySQL](https://github.com/jkk/honeysql), the absence of code bloat from the OO paradigm is noticeable and the functions are readable. Moving the focus to functions and away from custom types introduces simplicity. 


```
(-> (select :*) (from :BOOKS) sql/format) 
// ["SELECT * FROM BOOKS"]

(def  (-> (select :*) (from [:BOOKS :B]))))

(defn books-query-by-author [author] (-> books-query (merge-where  [:= :author author]) sql/format))

(books-query-by-author "Dave")
// ["SELECT * FROM BOOKS WHERE author = ?" "Dave"]                

(defn books-query-by-published-year [year] (-> books-query (merge-where  [:= :year year]) sql/format))

(books-query-by-published-year 1981)
// ["SELECT * FROM BOOKS WHERE year = ?" 1981]
     

(defn books-query-by (comp books-query merge-where))
```

The functional approach to interacting with a database is through the application of common functional principles. Domain knowledge of a specific library is not needed, as the solutions are the _very_ same solutions we use throughout the rest of our code base. The code is readable and expressive. The reduction in scaffolding eliminates complexity and potential bugs. 
