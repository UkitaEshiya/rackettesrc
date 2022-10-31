open CS17SetupRackette;
open Read.Reader;
open Types;


/* TODO: fill this with your initial top level environment,
 * consisting of built-in procedures like + or empty? */

let initialTle: environment => value =
env => BuiltinV(builtinData); 

let plus: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x+y 
    |_ => failwith ("invalid input")
  }; 

let add = {
  printedRep: "<builtin-proc-+>", 
  bProc: plus}; 


let subtraction: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x-y 
    |_ => failwith ("invalid input")
  }; 

let sub = {
  printedRep: "<builtin-proc-->", 
  bProc: subtraction}; 


let multiplication: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x*y 
    |_ => failwith ("invalid input")
  }; 

let mul = {
  printedRep: "<builtin-proc-*>", 
  bProc: multiplication}; 


let division: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x/y 
    |_ => failwith ("invalid input")
  }; 

let div = {
  printedRep: "<builtin-proc-/>", 
  bProc: division}; 


let remi: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x mod y 
    |_ => failwith ("invalid input")
  }; 

let rem = {
  printedRep: "<builtin-proc-rem>", 
  bProc: remi};


let eq: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x == y 
    |_ => failwith ("invalid input")
  }; 

let equa = {
  printedRep: "<builtin-proc-=>", 
  bProc: eq}; 


let great: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x > y 
    |_ => failwith ("invalid input")
  }; 

let greater = {
  printedRep: "<builtin-proc->>", 
  bProc: great}; 


let small: list(value) => int = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => x < y 
    |_ => failwith ("invalid input")
  }; 

let lesser = {
  printedRep: "<builtin-proc-<>", 
  bProc: small}; 



/*
+ , - , * , / , remainder , = , < , > , <= , >= , equal? , number? , zero? , 
cons , first , rest , empty? ,
cons? , and not ,
*/
7
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
      | NameE(name) 
      | AndE(expression, expression) 
      | OrE(expression, expression)
      | IfE(ifData)
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
let rec deflookup: (environment, name) => bool =
  (env, nom) =>
    switch (env) {
    | [(key, va), ...tl] =>
      if (key == nom) {
        failwith ("name already bind to value");
      } else {
        lookup(tl, nom);
      }
    | _ => true 
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
        if (deflookup(tle, nom)) {
          addBinding (tle, (nom, (eval (tle env expr))));
        } else {
          failwith ("name already bind to value");
        }

        
        
        | [expr, ...tl] => /* evaluate expression in environment*/
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
