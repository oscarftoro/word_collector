WORD COLLECTOR
================
[![Build Status](https://travis-ci.org/oscarftoro/word_collector.svg?branch=api)](https://travis-ci.org/oscarftoro/word_collector)

Hola, 
Word Collector (or WC or toilet) is a RESTful API that allows you to store words from foreign languages. 

Features
--------
* Create a word
* Retrieve a word
* Retrieve all words
* Edit a word
* Delete a word

Starting the application
------------------------
To start the application run on your terminal ```make shell```


Debugging the application
-------------------------
```make debug``` starts  the application using the error logger and allowing the use of a ?DEBUG macro to inspect the code in some of the modules where this macro is defined. This macro is an idea mentioned by Joe Armstrong in Programming Erlang, and Francesco Cesarini and Simon Thompson in Erlang Programming . I just wrote the Makefile that make this macro available. 

Running tests
-------------
Since the project uses erlang.mk, a simple ```make tests``` will do the job. There are Eunit and Common Tests included on this project.

When the tests are done, the results can easily be checked in the folder log. Open the index file and inspect what is wrong where. Common test mantain an history of your tests. Simply amazing. 


REST API
--------
Retrieve all words 

GET /wc/words

example:

```bash
curl -vX GET http://localhost:8080/wc/words
```
### Add a word
The word to be added has to be formated as a JSON object and has to convey the following order: title,language, definition.

POST /wc/words

*payload:*

{"word":{"title": "word title","language":"language name","definition": "word definition"}}

example:


```bash
curl -vX POST http://localhost:8080/wc/words \
-H"Content-Type:application/json" \
-d'{word: {"title": "hej","language":"dk", "definition": "hola" }}'
```
### Retrieve a word

GET /wc/words/word_title

example:


```bash
curl -vX GET http://localhost:8080/wc/words/hej
```
### Edit or Update a Word

PUT /wc/words/word\_title with a payload:
```javascript
{
    "word": "word title",
    "changes": {
        "definition": "new definition",
        "status" : "new status",
        "examples: "an example" }
}
```
example:
```bash
curl -vX PUT http://localhost:8080/wc/words/kiks \
-H"Content-Type:application/json" \
-d'{"word": "kiks","changes": {"definition": "galleturris",
"status" : "active","examples": "så skal vi spise kiks"}}'
```
