open CS17SetupRackette;
open Read.Reader;
open Types;


/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */

let initialTle: environment =
[(Name("+"), "<builtin-proc-+>"), (Name("-"), "<builtin-proc-->")] 

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseExpression */
let rec parseExpression: concreteProgramPiece => expression =
  input => failwith("parseExpression is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parseDefinition */
let parseDefinition: concreteProgramPiece => definition =
  input => failwith("parseDefinition is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe
 * and implement parsePiece */
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) => failwith("definitions not yet parsed")
    | _ => failwith("expressions not yet parsed")
    };

/* TODO: write the header comment parts required by the Design Recipe
 * for parse */
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* TODO: write the header comment parts required by the Design Recipe
 * and implement eval */
let rec eval: (tolLevelEnvt, localEnvt, expression) => value =
  (tle, env, expr) =>
      switch (exp) {
      | NumE(x) => NumV(x)
      | BoolE(bool) => BoolV(bool)
      | EmptyE => []
      | NameE(name) => /* return value bound to name */
      | AndE(expression, expression) => /* rule to how to evaluate and*/
      | OrE(expression, expression) => /* rule to how to evaluate or*/
      | IfE(ifData) => /* evaluate predicate, if true then evaluate first; if 
      false then evaluate no expression */ 
      | CondE(list(condData)) 
      | LambdaE(lambdaData)
      | LetE(letData)
      | ApplicationE(list(exp))=>
      }
      List.append(exprHelper(exp))
    /* NOTE: tle is top level environment and env is local environment */
    failwith("eval is not yet implemented");

/* TODO: write the header comment parts required by the Design Recipe */
let addDefinition: (environment, (name, expression)) => environment =
(env, (nom, expr)) => [(nom, expr), ...env]; 

/* TODO: write the header comment parts required by the Design Recipe
 * and implement stringOfValue*/
let rec stringOfValue: value => string =
  aValue => failwith("stringOfValue is not yet implemented");


/*
Procedure to lookup a definition in the environment, check whether it is already
bound to a value. Definition is name expression
*/
let rec deflookup: (environment, name) => option =
  (env, nom) =>
    switch (env) {
    | [(key, va), ...tl] =>
      if (key == nom) {
<<<<<<< HEAD
        Some(va);
=======
        failwith ("name already bound to value");
>>>>>>> 88ec93531fa60a30295d0462f6b0d8f4ce3e3ede
      } else {
        lookup(tl, nom);
      }
    | _ => None
    };


/*procedure to add new binding to environment*/
let addBinding: (environment, binding) => environment =
(env, bind) => [bind, ...env]; 

/* TODO: write the header comment parts required by the Design Recipe */
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [(nom, expr), ...tl] => 
        /* if definition, check whether name is already bind to value by looking
        up in environment; if already bind, return error saying name is already
        bind to value; if not bind, bind name to expression. */
        if {
          switch(deflookup(tle, nom)){
          | None => true
          | Some(result) => false
        }; 
        } {
          addDefinition(nom, tle); 
        } else {
          failwith ("name already bind to value");
        }

        
        
        | [expr, ...tl] => 
        /* if expression, evaluate expression to value.*/
          [eval(tle, env, e), processHelper(tle, tl)]
        }
    processHelper(initialTle, pieces);
  };

/* TODO: write the header comment parts required by the Design Recipe */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));
  
/* TODO: Test Cases (we have included a few sample check-expects) */
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(parseExpression(SymbolC("empty")), EmptyE,
  "parse empty expression");
// sample test: parseExpression with read
checkExpectExpression(parseExpression(read("empty")), EmptyE,
  "read and parse empty expression");
