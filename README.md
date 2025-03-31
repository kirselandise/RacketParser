# RacketParser
Trinity McCann | CS441 | Project 2

This is a recursive-descent parser for the following simple grammar inside of Racket

`program -> {stmt_list} $$ 
stmt_list -> stmt stmt_list

stmt_list -> epsilon 

stmt -> id = expr; 

    | if (expr) stmt_list endif; 

    | read id; 

    | write expr;

expr -> id etail 

   | num etail 

etail -> + expr 

   | - expr 

   | compare expr

   | epsilon

id -> [a-zA-Z]+

num -> numsign digit digit*

numsign -> + | - | epsilon 

compare -> < | <= | > | >= | == | !=`

The project required us to generate a functional recursive-descent parser using a LLM of choice. The following have been provided in the repository as per project guidelines:
- Original racket file containing the full code for the parser and automated testing.
- All recorded chat logs between the LLM and developer (Two chat logs were provided, both using Claude, one for development of the parser and the other for automated testing.)
- A comprehensive summary document outlining the development process, code overview, and testing. 
