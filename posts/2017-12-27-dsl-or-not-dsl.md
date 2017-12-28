---
title: dsl-or-not-dsl
summary: In this episode we discuss when is a good idea using a domain specific language in your code, what are the benefits and scenarios that can help with.
soundcloud: 361868087
author: Amir Barylko
---

## Why are DSL so tempting?

Domain specific languages have been in use for many many years in all kinds of software libraries.

There are many ways to implemnt a DSL. Starting from actually a complete language implementation to just a couple of functions that help us work with a particular domain.

To illustrate the options let us start with a very common DSL used for relational databases, yes you guessed it SQL.

SQL is the perfect example of _creating_ a language with a very well defined goal: query and modify the database. There is a special syntax to select columns from a table, join, etc.

The language is completely new to anybody that wants to learn how to use a database, no matter if you know Clojure, Scala, F# or Java you need to learn SQL in order to work with the database. 


## Getting real

Writing a new language could be a good idea but may not be easy or quick to implement. So the next best thing is to write some kind of DSL in our language of choice.

We talked about SQL and working with databases so why not explore how the [`jdbc`](https://github.com/clojure/java.jdbc) package works in `Clojure`.

Here the abstractions are very close to SQL and provide a _half_ way approach to work with the database.

Here is a bit of the `jdbc` documentation:

``` clojure
(def db-spec ... ) ;; Define a connection

;; DDL to create a table using keywords for table name and field names
(def fruit-table-ddl
  (jdbc/create-table-ddl :fruit [[:name "varchar(32)"]
                          [:appearance "varchar(32)"]
                          [:cost :int]
                          [:grade :real]]))

;; Execute the DDL and other commands using the connection spec
(jdbc/db-do-commands db-spec
                     [fruit-table-ddl
                      "CREATE INDEX name_ix ON fruit ( name );"])

;; CRUD operations are easy using hashmap as parameters and vectors to represent a command with parameters
(jdbc/insert! db-spec :table {:col1 42 :col2 "123"})               ;; Create
(jdbc/query   db-spec ["SELECT * FROM table WHERE id = ?" 13])     ;; Read
(jdbc/update! db-spec :table {:col1 77 :col2 "456"} ["id = ?" 13]) ;; Update
(jdbc/delete! db-spec :table ["id = ?" 13])                        ;; Delete

```

### One more step

Going further, though is a great start, we could get even more idiomatic to the language and define constructs that are well known and most developers fill comfortable using them.

To illustrate the point I am going to use [honeysql](https://github.com/jkk/honeysql) a high level library to write SQL commands and queries.

Honeysql uses a `hashmap` to represent a SQL query plus a formatting function to convert the map into a SQL string.

``` clojure
(require '[honeysql.core :as sql]
         '[honeysql.helpers :refer :all :as helpers])

(def sqlmap {:select [:a :b :c]
             :from [:foo]
             :where [:= :f.a "baz"]})

(sql/format sqlmap)

; => ["SELECT a, b, c FROM foo WHERE f.a = ?" "baz"]

```

But to make the experience really idiomatic there are helper functions to build queries using the thread first macro to combine the different components.

``` clojure
(-> (select :*)
    (from :foo)
    (where [:= :a 1] [:< :b 100])
    sql/format)

; => ["SELECT * FROM foo WHERE (a = ? AND b < ?)" 1 100]

```

Though `Honeysql` does not manage connections or execute commands we could add a small helper function to execute the query.

``` clojure
(-> (select :*)
    (from :foo)
    (where [:= :a 1] [:< :b 100])
    format-and-execute) ;; New custom function to format the query and execute with a connection pool

```

That way we can still use the thread first macro to construct the query and then get the results as well. We keep it idiomatic and simple.


### Testing repetition

Testing is another area that is a great candidate for a DSL. Writing scenarios and writing assertions for the results of those scenarios can be a very repetitive and tedious task.

Copying and pasting makes testing hard to maintain and unapealling. As an example, imagine we are writing an HTTP API and want to test that our endpoints behave as we expect.

Writing tests in this case is relatively simple because we just need to call the `api` function with the right parameters and assert that the response was as expected.

The test may look like:

``` clojure
(deftest customer-endpoint-test
  (testing "GET request to /customers returns all stored customers"
    (let [expected (create-some-customers)
          response (api (-> (mock/request :get "/api/customers")))]
      (if (ok? response)
        (let [actual (parse-body (:body response))]
          (assertion1 expected actual)
          (assertion2 expected actual)
          ;...
          (assertionN expected actual))))))
```

Every test is going to be very similar, and at some point it may get in the way between us and 'Ugh! Do I need to write another test?'

Thinking about DSLs we can draw some inspiration and invest some time in a macro to help us apply multiple assertions when the _response_ satisfies a predicate.

``` clojure
(deftest customer-endpoint-test
  (testing "GET request to /customers returns all stored customers"
    (let [expected (create-some-customers)]
      (-> (route-for :customers)
          (mock-request :get)
          api
          (assert-response ok? 
                           [assertion1 expected]
                           assertion2 
                           ; ... 
                           assertionN)))))
```

Or even further with a bit more effort:

``` clojure
(def-endpoint-test 
  :customers
  "GET request to /customers returns all stored customers"
  (-> (setup expected create-some-customers)
      (assert-ok [assertion1 expected]
                  assertion2 
                  ; ... 
                  assertionN)))
```

## The DSL connundrum

Sounds great! But if it is so easy, why not just do it all the time?

Well **because is not free**. 

How can we tell what is the right thing to do? How can we decide when we reach the inflexion point and we look at our code and wonder if a DSL would make things better or not?

On the upside the code would be easier to read, reason with and work with. On the downside we need to write more code, more testing, and that translates into more time. Also if we found a scenario we missed it could mean some serious refactoring.

Similar to good abstractions, a purposeful DSL will become clear after having a deep knowledge of a particular domain and solving the same problem several times.

However, while we learn about the domain and we identify patters we can start building small functions to rely on. Those are the stepping-stones to build something bigger that will represent even better the model of our problem to solve.

Combine that with continuous improvement and fearless refactoring and you will have a DSL in no time at all.




