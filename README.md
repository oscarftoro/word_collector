WORD COLLECTOR
================
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
To start the application run on your terminal```make shell```


Debugging the application
-------------------------
```
make debug
``` starts  the application using the error logger and allowing the use of a ?DEBUG macro to inspect the code in some of the modules where this macro is defined. This macro is an idea mentioned by Joe Armstrong in Programming Erlang, and Francesco Cesarini and Simon Thompson in Erlang Programming . I just wrote the Makefile that make this macro available. 

Running tests
-------------
Since the project use erlang.mk, a simple ```make tests``` will do the job.

When the tests are done, the results can easily checked in the folder log. Open the index file and inspect what is wrong where. Common test mantain an history of your tests. Simply amazing. To run the test call ```make tests```.

REST API
------------
Retrieve all words

GET /wc/words

example:
```bash
curl -vX GET http://localhost:8080/wc/words
```
### Add a word
The word to be added has to be formated as a JSON object.

PUT /wc/words {"title":"word1","definition":"palabra1"}

example:


```bash
curl -vX PUT http://localhost:8080/wc/words \
-H"Content-Type:application/json" \
-d'{ "title": "hej", "definition": "hola" }'
```
### Retrieve a word

GET /wc/words/word_title

example:


```bash
curl -vX GET http://localhost:8080/wc/words/hej
```
### Edit or Update a Word

POST /wc/words/word\_title with a payload:
```json
{
    "word": "word_title",
    "changes": {
        "definition": "new_definition",
        "status" : "new_status",
        "examples: "an_example"
        
    }
}
```
example:
```bash
curl -vX POST http://localhost:8080/wc/words/kiks \
-H"Content-Type:application/json" \
-d'{"word": "kiks","changes": {"definition": "galleturris",
"status" : "active","examples": "så skal vi spise kiks"}}'
```

