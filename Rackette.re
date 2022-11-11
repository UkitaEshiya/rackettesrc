open CS17SetupRackette;
open Read.Reader;
open Types;

/* procedures for the builtins */

/* for all output specifications, a failwith "invalid input" will be returned
   if the input does not fit the requirements of the builtin procedure. */

/* I/P: a list of two int values
 * O/P: integer value indicating the sum of the values */
let plus: list(value) => value =
  numlst => switch (numlst) {
           | [NumV(x), NumV(y)] => NumV(x + y)
           | _ => failwith("invalid input addition")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the difference of the values */
let subtraction: list(value) => value =
  numlst => switch (numlst) {
           | [NumV(x), NumV(y)] => NumV(x - y)
           | _ => failwith("invalid input subtraction")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the multiple of the values */
let multiplication: list(value) => value =
  numlst => switch (numlst) {
           | [NumV(x), NumV(y)] => NumV(x * y)
          | _ => failwith("invalid input multiplication")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the division of the values */
let division: list(value) => value =
  numlst => switch (numlst) {
           | [NumV(x), NumV(y)] => NumV(x / y)
           | _ => failwith("invalid input division")
    };

/* I/P: a list of two int values
 * O/P: integer value indicating the remainder of the first value divided by
 * the second */
let remi: list(value) => value =
  numlst => switch (numlst) {
            | [NumV(x), NumV(y)] => NumV(x mod y)
            | _ => failwith("invalid input remainder")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the two num values are equal */
let eq: list(value) => value =
  numlst => switch (numlst) {
            | [NumV(x), NumV(y)] => BoolV(x == y)
            | _ => failwith("invalid input num equal")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is greater than the
 * second */
let great: list(value) => value =
  numlst => switch (numlst) {
            | [NumV(x), NumV(y)] => BoolV(x > y)
            | _ => failwith("invalid input greater than")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is smaller than the
 * second */
let small: list(value) => value =
  numlst => switch (numlst) {
            | [NumV(x), NumV(y)] => BoolV(x < y)
            | _ => failwith("invalid input less than")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is greater than or
 * equal to the second */
let greq: list(value) => value =
  numlst => switch (numlst) {
            | [NumV(x), NumV(y)] => BoolV(x >= y)
            | _ => failwith("invalid input greater than or equal to")
    };

/* I/P: a list of two int values
 * O/P: boolean value indicating whether the first int is smaller than or
 * equal to the second */
let smeq: list(value) => value =
  numlst => switch (numlst) {
            | [NumV(x), NumV(y)] => BoolV(x <= y)
            | _ => failwith("invalid input less than or equal to")
    };

/* I/P: a list of two 'a values
 * O/P: boolean value indicating whether the two values are "structually
 * equal" */
let equality: list(value) => value =
  alst => switch (alst) {
          | [NumV(x), NumV(y)] => BoolV(x === y)
          | [BoolV(x), BoolV(y)] => BoolV(x === y)
          | [ListV(x), ListV(y)] => BoolV(x === y)
          | [_, _] => BoolV(false)
          | _ => failwith("invalid input equality")
    }; 

/* I/P: a value
 * O/P: boolean value indicating whether the input is a num value */
let isNum: list(value) => value =
  lst => switch (lst) {
        | [NumV(_)] => BoolV(true)
        | [_] => BoolV(false)
        | [_hd, ..._tl] => failwith("invalid input isnum must be one argument")
        | [] => failwith("invalid input isnum cannot be empty")
    };

/* I/P: a value
 * O/P: boolean value indicating whether the input is a zero */
let isZero: list(value) => value =
  lst => switch (lst) {
        | [NumV(0)] => BoolV(true)
        | [NumV(_)] => BoolV(false)
        | _ => failwith("invalid input is input zero, must be one argument num")
    };

/* I/P: two values, the second must be a list
 * O/P: a list value consisting of the first value followed by the second
 * value composed in a list */
let contain: list(value) => value =
  lst => switch (lst) {
        | [hd, ListV([])] => ListV([hd])
        | [hd, ListV([hd2, ...tl])] => ListV([hd, hd2, ...tl])
        | [] => failwith("invalid input expect two arguments")
        | [_hd] => failwith("invalid input expect two arguments")
        | [_hd, _] => failwith("invalid input second item must be list")
        | _ => failwith("invalid input expect two arguments")
    };

/* I/P: a list value
 * O/P: first item value of the list */
let firstLst: list(value) => value =
  lst => switch (lst) {
        | [] => failwith("invalid input first cannot be empty list")
        | [ListV([hd, ..._tl])] => hd
        | _ => failwith("invalid input first must be a list")
    };

/* I/P: a list value
 * O/P: the list except for the first item */
let restLst: list(value) => value =
  lst => switch (lst) {
        | [] => failwith("invalid input first cannot be empty list")
        | [ListV([_hd, ...tl])] => ListV(tl)
        | _ => failwith("invalid input rest must be a list")
    };

/* I/P: a list value
 * O/P: a boolean value indicate whether the list is empty */
let isEmpty: list(value) => value =
  lst => switch (lst) {
        | [ListV([])] => BoolV(true)
        | [ListV([_hd, ..._tl])] => BoolV(false)
        | _ => failwith("invalid input empty? must be a list")
    };

/* I/P: a list value
 * O/P: a boolean value indicate whether the list has item in there */
let isCons: list(value) => value =
  lst => switch (lst) {
        | [ListV([])] => BoolV(false)
        | [ListV([_hd, ..._tl])] => BoolV(true)
        | _ => failwith("invalid input cons? must be a list")
    };

/* I/P: a boolean value
 * O/P: the opposite boolean value */
let isNot: list(value) => value =
  lst => switch (lst) {
        | [BoolV(true)] => BoolV(false)
        | [BoolV(false)] => BoolV(true)
        | _ => failwith("invalid input not must be a boolean")
    };

/* InitialTle as a list of bindings */
let initialTle: environment = [
  (Name("+"), BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus})),
  (Name("-"), BuiltinV({printedRep: "<builtin-proc-->", bProc: subtraction}),),
  (Name("*"), 
    BuiltinV({printedRep: "<builtin-proc-*>", bProc: multiplication}),),
  (Name("/"), 
    BuiltinV({printedRep: "<builtin-proc-/>", bProc: division})),
  (Name("remainder"),
    BuiltinV({printedRep: "<builtin-proc-rem>", bProc: remi}),),
  (Name("="), BuiltinV({printedRep: "<builtin-proc-=>", bProc: eq})),
  (Name(">"), BuiltinV({printedRep: "<builtin-proc->>", bProc: great})),
  (Name("<"), BuiltinV({printedRep: "<builtin-proc-<>", bProc: small})),
  (Name("<="), BuiltinV({printedRep: "<builtin-proc-<=>", bProc: smeq})),
  (Name(">="), BuiltinV({printedRep: "<builtin-proc->=>", bProc: greq})),
  (Name("equal?"),
    BuiltinV({printedRep: "<builtin-proc-equal?>", bProc: equality}),),
  (Name("number?"),
    BuiltinV({printedRep: "<builtin-proc-number?>", bProc: isNum}),),
  (Name("zero?"),
    BuiltinV({printedRep: "<builtin-proc-zero?>", bProc: isZero}),),
  (Name("cons"),
    BuiltinV({printedRep: "<builtin-proc-cons>", bProc: contain}),),
  (Name("first"),
    BuiltinV({printedRep: "<builtin-proc-first>", bProc: firstLst}),),
  (Name("rest"),
    BuiltinV({printedRep: "<builtin-proc-rest>", bProc: restLst}),),
  (Name("empty?"),
    BuiltinV({printedRep: "<builtin-proc-empty?>", bProc: isEmpty}),),
  (Name("cons?"),
    BuiltinV({printedRep: "<builtin-proc-cons?>", bProc: isCons}),),
  (Name("not"), BuiltinV({printedRep: "<builtin-proc-not>", bProc: isNot})),
];

//lambdaHelp: ListC(list(concreteProgramPiece)) => list(NameE)
//input: input, a ListC(list(concreteProgramPiece))
//output: a list of NameE where each element corresponds to an element in 
//        input in the same order

// Base case: ListC([SymbolC(x)]) => [Name(x)]

// OI: ListC([SymbolC(name1), SymbolC(name2), SymbolC(name3)])
//    RI: ListC([SymbolC(name2), SymbolC(name3))])
//      RI: ListcC([SymbolC(name3)])
//      RO: [Name(name3)]
//    RO: [Name(name2), Name(name3)]
// OO: [Name(name1), Name(name2), Name(name3)]

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
//letpair in order

// Base case: ListC(ListC([SymbolC(name1), NumberC(1)])) => 
//             [{pairName: Name(name1), pairExpr: parseExpression(NumberC(1))}]

// OI: ListC(
//       ListC([SymbolC(name1), NumberC(1)]), 
//       ListC([SymbolC(name2), NumberC(2)]),
//       ListC([SymbolC(name3), NumberC(3)]))
//    RI: ListC(
//          ListC([SymbolC(name2), NumberC(2)]),
//          ListC([SymbolC(name3), NumberC(3)]))
//      RI: ListC(
//            ListC([SymbolC(name3), NumberC(3)]))
//      RO: [{pairName: Name(name3), pairExpr: parseExpression(NumberC(3))}]
//                ...
// OO: [[{pairName: Name(name1), pairExpr: parseExpression(NumberC(1))}],
//      [{pairName: Name(name2), pairExpr: parseExpression(NumberC(2))}],
//      [{pairName: Name(name3), pairExpr: parseExpression(NumberC(3))}]]

// In input, each pair of symbolC numberC must each have their own ListC. 

let rec letHelp: concreteProgramPiece => list(letPair) =
  input =>
    switch (input) {
    | ListC(x) =>
      switch (x) {
      | [ListC([SymbolC(nom), expr])] => 
      [{pairName: Name(nom), pairExpr: parseExpression(expr)},]
      | [ListC[SymbolC(nom), expr], ...tl] => 
      [{pairName: Name(nom), pairExpr: parseExpression(expr),}
      ,...letHelp(ListC(tl)),]
      | _ => failwith("invalid input letHelp needs ListC(SymbolC(string)...)")
      }
    | _ => failwith("letHelp must take in a list of concrete program pieces")
    }


//condHelp: list(concreteProgramPiece) => list(condData)
//input: a ListC of ListC[(condExpr, resultExpr)], representing a list of cond
//       result expression pairs
//output: a list of condData in the order of the input

// base case: [ListC([condExpr1, resultExpr1])] => 
//              [{conditionExpr: boolExpr1,
//                resultExpr: resultExpr1,}]
//short circuit here instead of []. 

// OI: ListC(
//       [ListC([condExpr1, resultExpr1])]
//       [ListC([condExpr2, resultExpr2])]
//       [ListC([condExpr3, resultExpr3])])
//    RI: ListC(
//          [ListC([condExpr2, resultExpr2])]
//          [ListC([condExpr3, resultExpr3])])
//      RI: ListC(
//            [ListC([condExpr3, resultExpr3])])
//      RO: [{conditionExpr: boolExpr3,
//            resultExpr: resultExpr3,}]
//                ...
// OO: [[{conditionExpr: boolExpr1,
//            resultExpr: resultExpr1,}],
//      [{conditionExpr: boolExpr2,
//            resultExpr: resultExpr2,}],
//      [{conditionExpr: boolExpr3,
//            resultExpr: resultExpr3,}]]

// each pair of expressions must each have their own ListC. 

and condHelp: list(concreteProgramPiece) => list(condData) =
  lst =>
    switch (lst) {
    | [] => failwith("cond needs a list of bool, expr pairs")
    | [ListC([condExpr, resultExpr])] => 
        [{conditionExpr: parseExpression(condExpr),
          resultExpr: parseExpression(resultExpr),},]
    | [ListC([condExpr, resultExpr]), ...tl] => 
        [{conditionExpr: parseExpression(condExpr),
          resultExpr: parseExpression(resultExpr),},
        ...condHelp(tl),]
    | _ => failwith("condHelp expects [ListC([...]...)]")
    }

/* parseExpression: concreteProgramPiece => expression
    input: cpe, a concreteProgramPiece
    output: an expression that corresponds to cpe where all rules for
    expressions are followed

  base case: when the input is either a single SymbolC(string) or NumberC(int). 
      depending on what the first cpp under ListC is, the following cpps are 
      handled differently based on the syntax and the representation of 
      each type of expressions. 

for parsing to an ApplicationE, where the first cpp is not one of the keywords, 
        OI: ListC([cpp1, cpp2, cpp3])
            RI: ListC([cpp2, cpp3])
              RI: ListC([cpp3])
              RO: ApplicationE([expr3])
            RO: ApplicationE([expr2, expr3])
        OO: ApplicationE([expr1, expr2, expr3])
   */
and parseExpression: concreteProgramPiece => expression =
  cpe =>
    switch (cpe) {
    | NumberC(x) => NumE(x)
    | SymbolC(x) =>
      switch (x) {
      | "true" => BoolE(true)
      | "false" => BoolE(false)
      | "empty" => EmptyE
      | x => NameE(Name(x))
      }
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
        | "cond" => CondE(condHelp([exp1, exp2]))
        | x =>
          ApplicationE([
            NameE(Name(x)),
            parseExpression(exp1),
            parseExpression(exp2),
          ])
        }
      | [SymbolC("and"), ..._] => failwith("and: expected two arguments")
      | [SymbolC("or"), ..._] => failwith("or: expected two arguments")
      | [SymbolC("lambda"), ..._] =>
        failwith("lambda: expected two arguments")
      | [SymbolC("let"), ..._] => failwith("let: expected two arguments")
      | [SymbolC("cond"), ...tl] => CondE(condHelp(tl))
      | [SymbolC(x)] => NameE(Name(x))
      | [x] => parseExpression(x) // short circuit for recursive calls 
      | [x, ...tl] =>
        ApplicationE([parseExpression(x), parseExpression(ListC(tl))])
      | [] => failwith("rackette cannot handle an empty list")
      }
    };

/* parseDefinition: concreteProgramPiece => definition
    input: cpd, a concreteProgramPiece
    output: an definition that corresponds to cpd where all rules for
    definitions are followed
   */
let parseDefinition: concreteProgramPiece => definition =
  cpd =>
    switch (cpd) {
    | ListC([SymbolC("define"), SymbolC(x), y]) => 
      (Name(x), parseExpression(y),)
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
    | ListC([SymbolC("define"), ..._]) => Definition(parseDefinition(input))
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
 Procedure to lookup a definition in the environment, check whether it is 
 already bound to a value. Definition is name expression.
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

let rec letPairHelper:
  (environment, environment, list(letPair)) => environment =
  (tle, local, lst) =>
    switch (lst) {
    | [] => []
    | [hd, ...tl] => 
        [(hd.pairName, eval(tle, local, hd.pairExpr)),
        ...letPairHelper(tle, local, tl)]
    }

/*eval: evaluate an expression with both the local and top level environment
  I/P: local environment, top level environment, an value
  O/P: the corresponding value after evaluation */
and eval: (environment, environment, expression) => value =
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
      switch (eval(tle, local, expr1)) {
      | BoolV(true) =>
        switch (eval(tle, local, expr2)) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("and expects two bool expr")
        }
      | BoolV(false) => BoolV(false)
      | _ => failwith("and expects two bool expr")
      }
    | OrE(expr1, expr2) =>
      switch (eval(tle, local, expr1)) {
      | BoolV(true) => BoolV(true)
      | BoolV(false) =>
        switch (eval(tle, local, expr2)) {
        | BoolV(false) => BoolV(false)
        | BoolV(true) => BoolV(true)
        | _ => failwith("or expects two bool expr")
        }
      | _ => failwith("or expects two bool expr")
      }
    | IfE(ifData1) =>
      switch (eval(tle, local, ifData1.boolExpr)) {
      | BoolV(true) => eval(tle, local, ifData1.trueExpr)
      | BoolV(false) => eval(tle, local, ifData1.falseExpr)
      | _ => failwith("if-expr must eval to bool")
      }
    | CondE([hd, ...tl]) =>
      switch (eval(tle, local, hd.conditionExpr)) {
      | BoolV(true) => eval(tle, local, hd.resultExpr)
      | BoolV(false) => eval(tle, local, CondE(tl))
      | _ => failwith("condition expr of cond must eval to bool")
      }

    | LambdaE(lambdaData1) =>
      ClosureV({
        cNameList: lambdaData1.nameList,
        cExpr: lambdaData1.lambdaBody,
        cEnv: local,
      })
    | LetE(letData1) =>
      eval(
        tle,
        extendEnv(letPairHelper(tle, local, letData1.letPairs), local),
        letData1.letBody,
      )
    | ApplicationE([hd, ...tl]) =>
      switch (eval(tle, local, hd)) {
      | BuiltinV(hd) => hd.bProc(List.map(eval(tle, []), tl))
     /* | ClosureV(hd) => eval(tle, extendEnv(hd.cEnv, local), hd.cExpr, ())*/
      | _ =>
        failwith
          ("syntax error for builtin proc or closure")
      }
    |_ => failwith ("cond expr or app expr cannot contain empty list")
    };
  };
/* NOTE: tle is top level environment and env is local environment */

/* addDefiniton: adding a definition, or a name expression, to an environment
   I/P: an environment and a definition in the format of name expression
   O/P: a new environment with the addition of the definition  */
let addDefinition: (environment, (name, expression)) => environment =
  (env, (nom, expr)) => {
    let newBinding = (nom, eval(env, env, expr));
    switch (defLookUp(env, nom)) {
    | Some(_) => failwith("name already bound to value")
    | None => [newBinding, ...env]
    };
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
    | ClosureV(_) => "user-defined procedure";
    
    };

/* Process:
   Input is an abstractProgram, which is a list of abstractProgram Pieces.
   Each abstractProgramPiece is checked to be either definition or expression.
   If definition, check whether name is already bind to value by looking
   up in environment; if already bind, return error saying name is already
   bind to value; if not bind, bind name to expression.
   If expression, evaluate expression to value.*/
let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => 
            [eval(tle, [], e),
            ...processHelper(tle, tl),]
        };
    processHelper(initialTle, pieces);
  };

/* I/P: a raw racket program in a string format
   O/P: a string indicating the corresponding of the racket program after
   evaluation */
let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

/* TEST CASES */
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

// string of value, eval, parseExpression with read on empty
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("empty")))),
  "empty",
  "read all the way for empty",
);

// eval parseExpression with read, single num
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("1")))),
  "1",
  "read all the way for a num",
);

//parseExpression with read on buildin proc
checkExpectExpression(
  parseExpression(read("(+ 1 5)")),
  ApplicationE([NameE(Name("+")), NumE(1), NumE(5)]),
  "read and parse 1 + 5",
);

//string of value for buildin proc
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("(+ 1 5)")))),
  "6",
  "stringOfValue buildin proc +",
);

//rackette for buildin proc
checkExpect(
  rackette("(+ 1 5)"), ["6"], "rackette of buildin proc"
  )

//read and parse for let
checkExpectExpression(
  parseExpression(read("(let ((x 5)) x)")),
  LetE({
    letPairs: [{pairName: Name("x"), pairExpr: NumE(5)}],
    letBody: NameE(Name("x")),
  }),
  "read and parse for let expression",
);

//read and parse for let
checkExpectExpression(
  parseExpression(read("(let ((x 5)(y 3))(+ x y))")),
LetE({letPairs: [{pairName: Name("x"), pairExpr: NumE(5), }, 
                 {pairName: Name("y"), pairExpr: NumE(3), }], 
    letBody: ApplicationE([NameE(Name("+")), 
            NameE(Name("x")), 
            NameE(Name("y"))]),}), 
  "read and parse for let expression",
);

//read and parse for lambda
checkExpectAbstractProgram(
  parse([read("(lambda (x) x)")]),
  [
    Expression(
      LambdaE({nameList: [Name("x")], lambdaBody: NameE(Name("x"))}),
    ),
  ],
  "read and parse for lambda expressions with one name",
);

//read and parse for lambda with two names
checkExpectAbstractProgram(
  parse([read("(lambda (x y) (+ x y))")]),
  [
    Expression(
      LambdaE({
        nameList: [Name("x"), Name("y")],
        lambdaBody:
          ApplicationE([
            NameE(Name("+")),
            NameE(Name("x")),
            NameE(Name("y")),
          ]),
      }),
    ),
  ],
  "read and parse for lambda expressions with two names",
);

//read and parse for if expression
checkExpectAbstractProgram(
  parse([read("(if (positive? -5) (3) 2)")]),
  [
    Expression(
      IfE({
        boolExpr: ApplicationE([NameE(Name("positive?")), NumE(-5)]),
        trueExpr: NumE(3),
        falseExpr: NumE(2),
        }),
    ),
  ],
  "read and parse for if statement",
);

//read and parse for and expression
checkExpectAbstractProgram(
  parse([read ("(and true 5)")]),
[Expression(AndE(BoolE(true), NumE(5)))],
  "read and parse for and statement",
);

checkExpect(
  rackette("(and true false)"),
  ["false"],
  "read and parse for and statement",
);

//read and parse for cond
checkExpectExpression(
  parseExpression(read("(cond
   ((positive? -5) 1)
   ((zero? -5) 2)
   ((positive? 5) 3))")),
  CondE([{conditionExpr: ApplicationE([NameE(Name("positive?")), NumE(-5)]), 
          resultExpr: NumE(1),}, 
          {conditionExpr: ApplicationE([NameE(Name("zero?")), NumE(-5)]), 
          resultExpr: NumE(2),}, 
          {conditionExpr: ApplicationE([NameE(Name("positive?")), NumE(5)]), 
          resultExpr: NumE(3),}]),
  "parseExpression for cond statement",
);

//string of value, eval, parseExpression for number?
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(number? 1)"))),
  ),
  "true",
  "check expect buildin proc is number",
);

//string of value, eval, parse expression for buildin proc remainder
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(remainder 4 2)"))),
  ),
  "0",
  "check expect buildin proc remainder",
);

//string of value, eval, parse expresion for buildin proc equal?
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(equal? 1 1)"))),
  ),
  "true",
  "check expect buildin proc equal?",
);

//string of value, eval, parse expression for cons
checkExpect(
  stringOfValue(
    eval(initialTle, [], parseExpression(read("(cons 1 empty)"))),
  ),
  "(cons 1 empty)",
  "check expect buildin proc cons",
);

//rackette of cond expr
checkExpect(
  rackette("(cond ((= 7 8) 6) (true 4))"),["4"], "rackette of cond expr"
  )

//test cases for all error
checkError(() => rackette("(+ 3 false)"), "invalid input addition");
checkError(() => rackette("(+ 3 false 4)"), 
  "syntax error for builtin proc or closure"); 
  //check why doens't faill with the error message for built ins
checkError(() => rackette("(- 3 4 5)"), 
  "syntax error for builtin proc or closure");
checkError(() => rackette("(* true false)"), "invalid input multiplication");
checkError(() => rackette("(/ true false)"), "invalid input division");
checkError(() => rackette("(= 3)"), "invalid input num equal");
checkError(() => rackette("(> 1)"), "invalid input greater than");
checkError(() => rackette("(< 1 true)"), "invalid input less than");
checkError(() => rackette("(>= 1)"), "invalid input greater than or equal to");
checkError(() => rackette("(<= 1)"), "invalid input less than or equal to");
checkError(() => rackette("(equal? 1 2 3)"), 
  "syntax error for builtin proc or closure");
checkError(() => rackette("(number? 1 2)"), 
  "invalid input isnum must be one argument");
checkError(() => rackette("(zero? 1 2)"), 
  "invalid input is input zero, must be one argument num");
checkError(() => rackette("(cons 1)"), 
  "invalid input expect two arguments");  
checkError(() => rackette("(cons 1 2)"), 
  "invalid input second item must be list");  
checkError(() => rackette("(first 1)"), 
  "invalid input first must be a list"); 
checkError(() => rackette("(rest 1)"), 
  "invalid input rest must be a list");   
checkError(() => rackette("(empty? 5)"), 
  "invalid input empty? must be a list");
checkError(() => rackette("(cons? 2)"), 
  "invalid input cons? must be a list"); 
checkError(() => rackette("(not 2)"), 
  "invalid input not must be a boolean");
checkError(() => rackette("()"), "rackette cannot handle an empty list");
checkError(() => rackette("(lambda x y)"), "invalid input lambdaHelp");
checkError(
  () => rackette("(let (4 x) 4)"),
  "invalid input letHelp needs ListC(SymbolC(string)...)",
);
checkError(
  () => rackette("(let x y)"),
  "letHelp must take in a list of concrete program pieces",
);
checkError(() => rackette("(let (x y))"), "let: expected two arguments");
checkError(
  () => rackette("(cond)"),
  "cond needs a list of bool, expr pairs",
);
checkError(() => rackette("(and true)"), "and: expected two arguments");
checkError(() => rackette("(and 5 true)"), "and expects two bool expr");
checkError(() => rackette("(and)"), "and: expected two arguments");
checkError(() => rackette("(or 5)"), "or: expected two arguments");
checkError(() => rackette("(or 5 6)"), "or expects two bool expr");
checkError(() => rackette("(lambda 5)"), "lambda: expected two arguments");
checkError(() => rackette("(lambda)"), "lambda: expected two arguments");
checkError(() => rackette("(let 5)"), "let: expected two arguments");
checkError(() => rackette("(let)"), "let: expected two arguments");
checkError(
  () => rackette("(define 6 7)"),
  "define expects variable name followed by expression: incorrect format",
);
checkError(() => rackette("(if 9 9 0)"), "if-expr must eval to bool");
checkError(
  () => rackette("(cond (false 6) (8 true))"),
  "condition expr of cond must eval to bool",
);
checkError(() => rackette("(if true)"), "expected three arguments");
checkError(
  () => rackette("(cond (false 6) (8 true))"),
  "condition expr of cond must eval to bool",
);
checkError(
  () => rackette("(cond (false 6) (8 7))"),
  "condition expr of cond must eval to bool",
);
checkError(
  () => rackette("(define x 7)(define x 8)"),
  "name already bound to value",
);
checkError(
  () => rackette("(define x 7)(define x 8)"),
  "name already bound to value",
);
checkError(
  () => rackette("(define x 7)(define x 8)"),
  "name already bound to value",
);
checkError(
  () => rackette("(define x 7)(define x 8)"),
  "name already bound to value",
);

checkError(
  () => rackette("(3 4 5)"),
  "syntax error for builtin proc or closure",
);