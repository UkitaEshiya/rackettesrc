open CS17SetupRackette;
open Read.Reader;
open Types;


/* procedures for the builtins */

/* for all output specifications, a failwith "invalid input" will be returned
if the input does not fit the requirements of the builtin procedure. */

  /* I/P: a list of two int values
   * O/P: integer value indicating the sum of the values */
let plus: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => NumV(x+y)
    |_ => failwith ("invalid input addition")
  }; 

  /* I/P: a list of two int values
   * O/P: integer value indicating the difference of the values */
let subtraction: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => NumV(x-y) 
    |_ => failwith ("invalid input subtraction")
  }; 

  /* I/P: a list of two int values
   * O/P: integer value indicating the multiple of the values */
let multiplication: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => NumV(x*y)
    |_ => failwith ("invalid input multiplication")
  }; 

  /* I/P: a list of two int values
   * O/P: integer value indicating the division of the values */
let division: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => NumV(x/y)
    |_ => failwith ("invalid input division")
  }; 

  /* I/P: a list of two int values
   * O/P: integer value indicating the remainder of the first value divided by 
   * the second */
let remi: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => NumV(x mod y)
    |_ => failwith ("invalid input remainder")
  }; 

  /* I/P: a list of two int values
   * O/P: boolean value indicating whether the two num values are equal */
let eq: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => BoolV(x == y)
    |_ => failwith ("invalid input num equal")
  }; 

  /* I/P: a list of two int values
   * O/P: boolean value indicating whether the first int is greater than the 
   * second */
let great: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => BoolV(x > y)
    |_ => failwith ("invalid input greater than")
  }; 

  /* I/P: a list of two int values
   * O/P: boolean value indicating whether the first int is smaller than the 
   * second */
let small: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => BoolV(x < y)
    |_ => failwith ("invalid input less than")
  }; 

  /* I/P: a list of two int values
   * O/P: boolean value indicating whether the first int is greater than or 
   * equal to the second */
let greq: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => BoolV(x >= y)
    |_ => failwith ("invalid input greater than or equal to")
  }; 

  /* I/P: a list of two int values
   * O/P: boolean value indicating whether the first int is smaller than or 
   * equal to the second */
let smeq: list(value) => value = numlst => 
  switch (numlst) {
    |[NumV(x), NumV(y)] => BoolV(x <= y)
    |_ => failwith ("invalid input less than or equal to")
  }; 

  /* I/P: a list of two 'a values
   * O/P: boolean value indicating whether the two values are "structually
   * equal" */
let equality: list(value) => value = alst => 
  switch (alst) {
    |[NumV(x), NumV(y)] => BoolV(x === y)
    |[BoolV(x), BoolV(y)] => BoolV(x === y)
    |[ListV(x), ListV(y)] => BoolV(x === y)
    |[_, _] => BoolV(false) 
    |_ => failwith ("invalid input equality")
  }; /* might need to evaluate when its a BuiltinV and ClosureV to check for the 
  equality of the values indicated by the data */

  /* I/P: a value
   * O/P: boolean value indicating whether the input is a num value */
let isnum: value => value = va => 
  switch (va) {
    |NumV(_) => BoolV(true)
    |_ => BoolV(false)
    |[_hd, ..._tl] => failwith ("invalid input is input num")
  }; 

  /* I/P: a value
   * O/P: boolean value indicating whether the input is a zero */
let iszero: value => value = va => 
  switch (va) {
    |NumV(0) => BoolV(true)
    |NumV(_) => BoolV(false)
    |_ => failwith ("invalid input is input zero")
  }; 

  /* I/P: two values, the second must be a list
   * O/P: a list value consisting of the first value followed by the second 
   * value composed in a list */
let contain: (value, value) => value = (item, lst) => 
  switch (lst) {
    |ListV([]) => ListV([item])
    |ListV([_hd, ..._tl]) => ListV([item, _hd, ..._tl])
    |_ => failwith ("invalid input second item must be list")
  }; 

  /* I/P: a list value
   * O/P: first item value of the list */
let firstlst: value => value = lst => 
  switch (lst) {
    |ListV([]) => failwith ("invalid input first cannot be empty list")
    |ListV([hd, ..._tl]) => hd
    |_ => failwith ("invalid input input must be a list")
  }; 

  /* I/P: a list value
   * O/P: the list except for the first item */
let restlst: value => value = lst => 
  switch (lst) {
    |ListV([]) => failwith ("invalid input first cannot be empty list")
    |ListV([_hd, ...tl]) => ListV(tl)
    |_ => failwith ("invalid input input must be a list")
  }; 

  /* I/P: a list value
   * O/P: a boolean value indicate whether the list is empty */
let isempty: value => value = lst => 
  switch (lst) {
    |ListV([]) => BoolV(true)
    |ListV([_hd, ..._tl]) => BoolV(false)
    |_ => failwith ("invalid input input must be a list")
  }; 

  /* I/P: a list value
   * O/P: a boolean value indicate whether the list has item in there */
let iscons: value => value = lst => 
  switch (lst) {
    |ListV([]) => BoolV(false)
    |ListV([_hd, ..._tl]) => BoolV(true)
    |_ => failwith ("invalid input input must be a list")
  }; 

  /* I/P: a boolean value
   * O/P: the opposite boolean value */
let isnot: value => value = va => 
  switch (lst) {
    |BooV(true) => BoolV(false)
    |BoolV(false) => BoolV(true)
    |_ => failwith ("invalid input input must be a boolean")
  }; 

/* InitialTle as a list of bindings */
let initialTle: environment = [
  (
    Name("+"), BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus})
  ),
  (
    Name("-"),
    BuiltinV({printedRep: "<builtin-proc-->", bProc: subtraction}),
  ),
  (
    Name("*"),
    BuiltinV({printedRep: "<builtin-proc-*>", bProc: multiplication}),
  ),
  (
    Name("/"),
    BuiltinV({printedRep: "<builtin-proc-/>", bProc: division}),
  ),
  (
    Name("remainder"),
    BuiltinV({printedRep: "<builtin-proc-rem>", bProc: remi}),
  ),
  (
    Name("="),
    BuiltinV({printedRep: "<builtin-proc-=>", bProc: eq}),
  ),
  (
    Name(">"),
    BuiltinV({printedRep: "<builtin-proc->>", bProc: great}),
  ),
  (
    Name("<"),
    BuiltinV({printedRep: "<builtin-proc-<>", bProc: small}),
  ),
  (
    Name("<="),
    BuiltinV({printedRep: "<builtin-proc-<=>", bProc: greq}),
  ),
  (
    Name(">="),
    BuiltinV({printedRep: "<builtin-proc->=>", bProc: smeq}),
  ),
  (
    Name("equal?"),
    BuiltinV({printedRep: "<builtin-proc-equal?>", bProc: equality}),
  ),
  (
    Name("number?"),
    BuiltinV({printedRep: "<builtin-proc-number?>", bProc: isnum}),
  ),
  (
    Name("zero?"),
    BuiltinV({printedRep: "<builtin-proc-zero?>", bProc: iszero}),
  ),
  (
    Name("cons"),
    BuiltinV({printedRep: "<builtin-proc-cons>", bProc: contain}),
  ),
  (
    Name("first"),
    BuiltinV({printedRep: "<builtin-proc-first>", bProc: firstlst}),
  ),
  (
    Name("rest"),
    BuiltinV({printedRep: "<builtin-proc-rest>", bProc: restlst}),
  ),
  (
    Name("empty?"),
    BuiltinV({printedRep: "<builtin-proc-empty?>", bProc: isempty}),
  ),
  (
    Name("cons?"),
    BuiltinV({printedRep: "<builtin-proc-cons?>", bProc: iscons}),
  ),
  (
    Name("not"),
    BuiltinV({printedRep: "<builtin-proc-not>", bProc: isnot}),
  ),
];


/*
+ , - , * , / , remainder , = , < , > , <= , >= , equal? , number? , zero? , 
cons , first , rest , empty? ,
cons? , not 
*/

/* parseExpression: concreteProgramPiece => expression
 input: cpe, a concreteProgramPiece
 output: an expression that corresponds to cpe where all rules for expressions are followed
*/
let rec parseExpression: concreteProgramPiece => expression =
  cpe =>
    switch (cpe) {
    | NumberC(x) => NumE(x)
    | ListC(SymbolC("if"), ifEx, yes, no) =>
      IfE(parseExpression(ifEx), parseExpression(yes), parseExpression(no))
    | ListC(SymbolC("and"), pie1, pie2) =>
      AndE(parseExpression(pie1), parseExpression(pie2))
    | ListC(SymbolC("or"), pie1, pie2) =>
      OrE(parseExpression(pie1), parseExpression(pie2))
    | ListC(SymbolC("cond"),    ) => CondE([{,}]) // come back to this!!!
    | ListC(SymbolC("lambda"), nameLst, body) => 
      LambaE({nameList:/*HELPER DETAILED BELOW*/, lambdaBody: parseExpression(pie2)})
    | ListC(SymbolC("let"), letPairLst, rest) => 
      LetE({letPairs: /*HELPER DETAILED BELOW*/, parseExpression(rest)})
    | ListC(x,..tl) => ApplicationE(parseExpression(x), parseExpression(tl)) /* <---- POTENTIAL HELPER DETAILED BELOW bc tl is a list not listC so parsing wont work*/
    | SymbolC(x)
      switch(x) {
        | "true" => BoolE(true)
        | "false" => Bool(false)
        | "empty" => EmptyE
        | x => NameE(Name(x))
      } 
    | _ => failwith("syntax error") //ask on edstem about failwith specifics
  
    };

    //helper takes in list c of symbols that are just names to create a list of names spec for lambda data 
      // lambda ListC(Symbolx symboly) (...)
      //  | ListC(x,...tl) => [NameE(Name("x"),... parseExpression(ListC(tl))] IDEATION
    //similar for letpairlist
    //helper that maps procedure over the arguments of application e switch case because it could have 2,3,4,5 etc
    // number of arguments especially if it is a userdefied closure and we dont know how many there will be so we 
    //cant write an accurate switch case

/* parseExpression: concreteProgramPiece => definition
 input: cpd, a concreteProgramPiece
 output: an definition that corresponds to cpd where all rules for definitions are followed
*/

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

/* extendEnv: procedure to extend one env by another, here Tle by local
I/P: two environments, one top and one local
O/P: a new environment, with the tle extended by the local*/
let extendEnv: (environment, environment) => environment =
(tle, local) => List.append(tle, local); 

/*
Procedure to lookup a definition in the environment, check whether it is already
bound to a value. Definition is name expression
*/
let rec deflookup: (environment, name) => option(value) =
  (env, nom) =>
    switch (env) {
    | [(key, va), ...tl] =>
      if (key == nom) {
        Some(va);
      } else {
        lookup(tl, nom);
      }
    | _ => None
    };

/* TODO: write the header comment parts required by the Design Recipe
 * and implement eval */
let rec eval: (tolLevelEnvt, localEnvt, expression) => value =
  (tle, local, expr) => {
      let allenv = extendEnv (tle, local); 
      switch (exp) {
      | NumE(x) => NumV(x)
      | BoolE(bool) => BoolV(bool)
      | EmptyE => []
      | NameE(name) => 
        if (deflookup (allenv, name) == Some(va)) {
          va; 
        } else {
          failwith ("name not bounded to value, cannot eval")
        }
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
    failwith("eval is not yet implemented")};


/* addDefiniton: adding a definition, or a name expression, to an environment
  I/P: an environment and a definition in the format of name expression
  O/P: a new environment with the addition of the definition  */
let addDefinition: (environment, (name, expression)) => environment =
  (env, (nom, expr)) =>
    if (deflookup(env, nom) == Some(va)) {
      failwith("name already bound to value");
    } else {
      [(nom, expr), ...env];
    };

/* TODO: write the header comment parts required by the Design Recipe
 * and implement stringOfValue*/
let rec stringOfValue: value => string =
  aValue => failwith("stringOfValue is not yet implemented");


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
      /* recursive call for processhelper tl of list, then helper for add definition*/
        
        
        | [expr, ...tl] => 
        /* if expression, evaluate expression to value.*/
          [eval(tle, env, e), ... processHelper(tle, tl)]
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
