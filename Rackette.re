open CS17SetupRackette;
open Read.Reader;
open Types;

/* procedures for the builtins */

/* for all output specifications, a failwith "invalid input" will be returned
   if the input does not fit the requirements of the builtin procedure. */

/* I/P: a list of two int values
 * O/P: integer value indicating the sum of the values */
let plus: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => NumV(x + y)
    | _ => failwith("invalid input addition")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the difference of the values */
let subtraction: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => NumV(x - y)
    | _ => failwith("invalid input subtraction")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the multiple of the values */
let multiplication: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => NumV(x * y)
    | _ => failwith("invalid input multiplication")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the division of the values */
let division: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => NumV(x / y)
    | _ => failwith("invalid input division")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the remainder of the first value divided by
 * the second */
let remi: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => NumV(x mod y)
    | _ => failwith("invalid input remainder")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the two num values are equal */
let eq: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => BoolV(x == y)
    | _ => failwith("invalid input num equal")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is greater than the
 * second */
let great: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => BoolV(x > y)
    | _ => failwith("invalid input greater than")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is smaller than the
 * second */
let small: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => BoolV(x < y)
    | _ => failwith("invalid input less than")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is greater than or
 * equal to the second */
let greq: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => BoolV(x >= y)
    | _ => failwith("invalid input greater than or equal to")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is smaller than or
 * equal to the second */
let smeq: list(value) => value =
  numlst =>
    switch (numlst) {
    | [NumV(x), NumV(y)] => BoolV(x <= y)
    | _ => failwith("invalid input less than or equal to")
    };

/* I/P: a list of two 'a values
 * O/P: boolean value indicating whether the two values are "structually
 * equal" */
let equality: list(value) => value =
  alst =>
    switch (alst) {
    | [NumV(x), NumV(y)] => BoolV(x === y)
    | [BoolV(x), BoolV(y)] => BoolV(x === y)
    | [ListV(x), ListV(y)] => BoolV(x === y)
    | [_, _] => BoolV(false)
    | _ => failwith("invalid input equality")
    }; /* might need to evaluate when its a BuiltinV and ClosureV to check for the
  equality of the values indicated by the data */

/* I/P: a value
 * O/P: boolean value indicating whether the input is a num value */
let isNum: list(value) => value =
  lst =>
    switch (lst) {
    | [NumV(_)] => BoolV(true)
    | [_] => BoolV(false)
    | [_hd, ..._tl] => failwith("invalid input isnum must be one argument")
    | [] => failwith("invalid input isnum cannot be empty")
    };

/* I/P: a value
 * O/P: boolean value indicating whether the input is a zero */
let isZero: list(value) => value =
  lst =>
    switch (lst) {
    | [NumV(0)] => BoolV(true)
    | [NumV(_)] => BoolV(false)
    | _ => failwith("invalid input is input zero, must be one argument num")
    };

/* I/P: two values, the second must be a list
 * O/P: a list value consisting of the first value followed by the second
 * value composed in a list */
let contain: list(value) => value =
  lst =>
    switch (lst) {
    | [hd, ListV([])] => ListV([hd])
    | [hd, ListV([hd2, ...tl])] => ListV([hd, hd2, ...tl])
    | [] => failwith("invalid input expect two arguments")
    | [hd] => failwith("invalid input expect two arguments")
    | [hd, _] => failwith("invalid input second item must be list")
    | _ => failwith("invalid input expect two arguments")
    };

/* I/P: a list value
 * O/P: first item value of the list */
let firstLst: list(value) => value =
  lst =>
    switch (lst) {
    | [] => failwith("invalid input first cannot be empty list")
    | [ListV([hd, ..._tl])] => hd
    | _ => failwith("invalid input input must be a list")
    };

/* I/P: a list value
 * O/P: the list except for the first item */
let restLst: list(value) => value =
  lst =>
    switch (lst) {
    | [] => failwith("invalid input first cannot be empty list")
    | [ListV([_hd, ...tl])] => ListV(tl)
    | _ => failwith("invalid input input must be a list")
    };

/* I/P: a list value
 * O/P: a boolean value indicate whether the list is empty */
let isEmpty: list(value) => value =
  lst =>
    switch (lst) {
    | [ListV([])] => BoolV(true)
    | [ListV([_hd, ..._tl])] => BoolV(false)
    | _ => failwith("invalid input input must be a list")
    };

/* I/P: a list value
 * O/P: a boolean value indicate whether the list has item in there */
let isCons: list(value) => value =
  lst =>
    switch (lst) {
    | [ListV([])] => BoolV(false)
    | [ListV([_hd, ..._tl])] => BoolV(true)
    | _ => failwith("invalid input input must be a list")
    };

/* I/P: a boolean value
 * O/P: the opposite boolean value */
let isNot: list(value) => value =
  lst =>
    switch (lst) {
    | [BoolV(true)] => BoolV(false)
    | [BoolV(false)] => BoolV(true)
    | _ => failwith("invalid input input must be a boolean")
    };

/* InitialTle as a list of bindings */
let initialTle: environment = [
  (Name("+"), BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus})),
  (
    Name("-"),
    BuiltinV({printedRep: "<builtin-proc-->", bProc: subtraction}),
  ),
  (
    Name("*"),
    BuiltinV({printedRep: "<builtin-proc-*>", bProc: multiplication}),
  ),
  (Name("/"), BuiltinV({printedRep: "<builtin-proc-/>", bProc: division})),
  (
    Name("remainder"),
    BuiltinV({printedRep: "<builtin-proc-rem>", bProc: remi}),
  ),
  (Name("="), BuiltinV({printedRep: "<builtin-proc-=>", bProc: eq})),
  (Name(">"), BuiltinV({printedRep: "<builtin-proc->>", bProc: great})),
  (Name("<"), BuiltinV({printedRep: "<builtin-proc-<>", bProc: small})),
  (Name("<="), BuiltinV({printedRep: "<builtin-proc-<=>", bProc: greq})),
  (Name(">="), BuiltinV({printedRep: "<builtin-proc->=>", bProc: smeq})),
  (
    Name("equal?"),
    BuiltinV({printedRep: "<builtin-proc-equal?>", bProc: equality}),
  ),
  (
    Name("number?"),
    BuiltinV({printedRep: "<builtin-proc-number?>", bProc: isNum}),
  ),
  (
    Name("zero?"),
    BuiltinV({printedRep: "<builtin-proc-zero?>", bProc: isZero}),
  ),
  (
    Name("cons"),
    BuiltinV({printedRep: "<builtin-proc-cons>", bProc: contain}),
  ),
  (
    Name("first"),
    BuiltinV({printedRep: "<builtin-proc-first>", bProc: firstLst}),
  ),
  (
    Name("rest"),
    BuiltinV({printedRep: "<builtin-proc-rest>", bProc: restLst}),
  ),
  (
    Name("empty?"),
    BuiltinV({printedRep: "<builtin-proc-empty?>", bProc: isEmpty}),
  ),
  (
    Name("cons?"),
    BuiltinV({printedRep: "<builtin-proc-cons?>", bProc: isCons}),
  ),
  (Name("not"), BuiltinV({printedRep: "<builtin-proc-not>", bProc: isNot})),
];

/*
 + , - , * , / , remainder , = , < , > , <= , >= , equal? , number? , zero? ,
 cons , first , rest , empty? ,
 cons? , not
 */

//lambdaHelp: ListC(list(concreteProgramPiece)) => list(NameE)
//input: input, a ListC(list(concreteProgramPiece))
//output: a list of NameE where each element corresponds to an element in input in the same order
let rec lambdaHelp: concreteProgramPiece => list(name) =
  listC =>
    switch (listC) {
    | ListC([SymbolC(x)]) => [Name(x)]
    | ListC([SymbolC(x), ...tl]) => [Name(x), ...lambdaHelp(ListC(tl))]
    | _ => failwith("invalid input lambdaHelp")
    };

//letHelp: ListC(list(concreteProgramPiece)) => list(letPairs)
//input: ListC(input), ListC(list(concreteProgramPiece))
//output: a list of let pairs where each list in input is translated into a
//letpair in ordef */
let rec letHelp: concreteProgramPiece => list(letPair) =
  listC =>
    switch (listC) {
    | ListC([]) => [] /*name exp is not a concrete
                                                        program piece*/

    | _ => []
    }

/* let rec letHelp: ListC(list(concreteProgramPiece)) => list(letPairs) =
   ListC(input) =>
      switch(input) {
        |[[name, exp]] => [{NameE(Name(name))]
        |[[name, exp],...tl] => [{NameE(Name(name)), parseExpression(exp)},...
        letHelp(ListC)]
      } */

and condHelp: list(concreteProgramPiece) => list(condData) =
  /// does cond send in ListC of lists pr List C of List C or List pf List c?
  lst =>
    switch (lst) {
    | [] => []
    | [ListC([condExpr, resultExpr])] => [
        {
          conditionExpr: parseExpression(condExpr),
          resultExpr: parseExpression(resultExpr),
        },
      ]
    | [ListC([condExpr, resultExpr]), ...tl] => [
        {
          conditionExpr: parseExpression(condExpr),
          resultExpr: parseExpression(resultExpr),
        },
        ...condHelp(tl),
      ]
    | _ =>
      failwith(
        "cond expect two arguments, but found incorrect number of
      expressions following cond",
      )
    }

/* parseExpression: concreteProgramPiece => expression
    input: cpe, a concreteProgramPiece
    output: an expression that corresponds to cpe where all rules for
    expressions are followed
   */
and parseExpression: concreteProgramPiece => expression =
  cpe =>
    switch (cpe) {
    | NumberC(x) => NumE(x)
    | ListC(lst) =>
      switch (lst) {
      | [SymbolC("if"), ifEx, yes, no] =>
        IfE({
          boolExpr: parseExpression(ifEx),
          trueExpr: parseExpression(yes),
          falseExpr: parseExpression(no),
        })
      | [SymbolC("if"), ..._] => failwith("expected three arguments")
      | [SymbolC(x), exp1, exp2] =>
        switch (x) {
        | "and" => AndE(parseExpression(exp1), parseExpression(exp2))
        | "or" => OrE(parseExpression(exp1), parseExpression(exp2))
        | "lambda" =>
          LambdaE({
            nameList: lambdaHelp(exp1),
            lambdaBody: parseExpression(exp2),
          })
        | "let" =>
          LetE({letPairs: letHelp(exp1), letBody: parseExpression(exp2)})
        | _ => failwith("misc invalid input")
        }
      | [SymbolC(x), exp1, exp2] =>
        switch (x) {
        | "and" => failwith("and: expected two arguments")
        | "or" => failwith("or: expected two arguments")
        | "lambda" => failwith("lambda: expected two arguments")
        | "let" => failwith("let: expected two arguments")
        | _ => failwith("misc invalid input")
        }
      | [SymbolC("cond"), ...tl] => CondE(condHelp(tl))
      | [SymbolC(x)] => NameE(Name(x))
      | [SymbolC(x), ...tl] =>
        ApplicationE([NameE(Name(x)), parseExpression(ListC(tl))])
      | _ => failwith("misc invalid input")
      }
    | SymbolC(x) =>
      switch (x) {
      | "true" => BoolE(true)
      | "false" => BoolE(false)
      | "empty" => EmptyE
      | x => NameE(Name(x))
      }
    | _ =>
      failwith("entered concreteProgramPice is not syntactically correct")
    };

/* parseDefinition: concreteProgramPiece => definition
    input: cpd, a concreteProgramPiece
    output: an definition that corresponds to cpd where all rules for
    definitions are followed
   */
let parseDefinition: concreteProgramPiece => definition =
  cpd =>
    switch (cpd) {
    | ListC([SymbolC("define"), SymbolC(x), y]) => (
        Name(x),
        parseExpression(y),
      )
    | _ =>
      failwith(
        "define expects variable name followed by expression: incorrect format",
      )
    };

//parsePiece: concreteProgramPiece => abstractProgramPiece
//input: cpp, a concreteProgramPiece
//output: the coordinating abstractProgramPiece either Definition or Expression
let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    };

//parse: concreteProgram => abstractProgram
//input: input, a concreteProgram
//output: an abstract program which corresponds to input
let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input);

/* extendEnv: procedure to extend one env by another, here Tle by local
   I/P: two environments, one top and one local
   O/P: a new environment, with the tle extended by the local*/
let extendEnv: (environment, environment) => environment =
  (local, tle) => List.append(local, tle);

/*
 Procedure to lookup a definition in the environment, check whether it is already
 bound to a value. Definition is name expression.
 I/P: an environment and a name
 O/P: the corresoding value to the name; None if name not bounded to a value
 */
let rec defLookUp: (environment, name) => option(value) =
  (env, nom) =>
    switch (env) {
    | [(key, va), ...tl] =>
      if (key == nom) {
        Some(va);
      } else {
        defLookUp(tl, nom);
      }
    | _ => None
    };

/* procedure to make letData into environment
   let rec letPairHelper: list(letPair) => environment =
     lst =>
       switch (lst) {
       | [] => []
       | [hd, ...tl] => [
           (hd.pairName, eval(hd.pairExpr)),
           ...letPairHelper(tl),
         ]
       }; need to eval the expression in let pair to produce value*/

/*eval: evaluate an expression with both the local and top level environment
  I/P: local environment, top level environment, an value
  O/P: the corresponding value after evaluation */

let rec eval: (environment, environment, expression) => value =
  (tle, local, expr) => {
    let allEnv = extendEnv(local, tle);
    switch (expr) {
    | NumE(x) => NumV(x)
    | BoolE(boolExpr) => BoolV(boolExpr)
    | EmptyE => ListV([])
    | NameE(nom) =>
      switch (defLookUp(allEnv, nom)) {
      | Some(va) => va
      | None => failwith("name not bounded to value, cannot eval")
      }
    | AndE(expr1, expr2) =>
      if (eval(tle, local, expr1) == BoolV(true)
          && eval(tle, local, expr2) == BoolV(true)) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | OrE(expr1, expr2) =>
      if (eval(tle, local, expr1) == BoolV(true)
          || eval(tle, local, expr2) == BoolV(true)) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | IfE(ifData1) =>
      /* evaluate predicate, if true then evaluate first;
         if false then evaluate second expression */
      if (eval(tle, local, ifData1.boolExpr) == BoolV(true)) {
        eval(tle, local, ifData1.trueExpr);
      } else {
        eval(tle, local, ifData1.falseExpr);
      }
    | CondE([hd, ...tl]) =>
      if (eval(tle, local, hd.conditionExpr) == BoolV(true)) {
        eval(tle, local, hd.resultExpr);
      } else {
        eval(tle, local, CondE(tl));
      }
    | LambdaE(lambdaData1) =>
      ClosureV({
        cNameList: lambdaData1.nameList,
        cExpr: lambdaData1.lambdaBody,
        cEnv: local,
      })
    | LetE(letData1) =>
      eval(tle, extendEnv(letPairHelper(letData1), local), letData1.letBody)
    | ApplicationE(list) => []
    };
  };
/*    List.append(exprHelper(exp)) */
/* NOTE: tle is top level environment and env is local environment */

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

/* stringOfValue: procedure to convert the value from evaluation into a string
   I/P: a value
   O/P: the corresponding string representing the value*/
let rec stringOfValue: value => string =
  aValue =>
    switch (aValue) {
    | NumV(x) => string_of_int(x)
    | BoolV(boo) => string_of_bool(boo)
    | ListV([hd, ...tl]) =>
      "(cons "
      ++ stringOfValue(hd)
      ++ " "
      ++ stringOfValue(ListV(tl))
      ++ ")"
    | ListV([]) => "empty"
    | BuiltinV(builtin) => builtin.printedRep
    /*  | ClosureV(closureData) => failwith ("implement later") */
    };

/*procedure to add new binding to environment, not part of original file
  I/P: an environment and a binding
  O/P: the new environment with the binding added */
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
          if ({
                switch (deflookup(tle, nom)) {
                | None => true
                | Some(result) => false
                };
              }) {
            addDefinition(nom, tle);
          } else {
            failwith("name already bind to value");
          }
        /* recursive call for processhelper tl of list, then helper for add definition*/

        | [expr, ...tl] =>
          /* if expression, evaluate expression to value.*/
          [eval(tle, env, e), ...processHelper(tle, tl)]
        };
    processHelper(initialTle, pieces);
  };

/* I/P: a raw racket program in a string format
   O/P: a string indicating the corresponding of the racket program after
   evaluation */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* TODO: Test Cases (we have included a few sample check-expects) */
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);
// sample test: parseExpression with read
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parse empty expression",
);
