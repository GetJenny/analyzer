This package is being integrated in `*chat`, it will soon deleted

# Analyzer language and functions

Temporary project to implement a language to be integrated into *chat for state retrieval.

In a nutshell, if chat developers wants the user to go into state "forgot_password" whenever all this conditions are satisfied:

* the query is at least 22 character long
* there is the word "password"
* there is: either the word "forgot" or the word "lost" or a synonym of the word "problem"

we want them to be able to write something like that:

`and(regex(".{22,}"), and(or(keyword("forgot"), keyword("lost"), synonym("problem")), keyword("password")))`

into a state field.

The developer only needs to know that the funcions regex, keyword etc exist and know how to use the boolean operators `and`, `or` and `not`.
