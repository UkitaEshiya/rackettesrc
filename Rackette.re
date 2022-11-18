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

// OI: ListC([SymbolC(name1), SymbolC(name2), 
//        SymbolC(name3), SymbolC(name4), SymbolC(name5))])
//    RI: ListC([SymbolC(name1), SymbolC(name2), 
//            SymbolC(name3), SymbolC(name4)]))
//               ...
//    RO:  [Name(name2), Name(name3), Name(name4), Name(name5)]
// OO: [Name(name1), Name(name2), Name(name3), Name(name4), Name(name5)]

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

// OI: ListC(
//       ListC([SymbolC(name1), NumberC(1)]), 
//       ListC([SymbolC(name2), NumberC(2)]),
//       ListC([SymbolC(name3), NumberC(3)]),
//       ListC([SymbolC(name4), NumberC(4)]), 
//       ListC([SymbolC(name5), NumberC(5)]))
//    RI: ListC(
//       ListC([SymbolC(name2), NumberC(2)]),
//       ListC([SymbolC(name3), NumberC(3)]),
//       ListC([SymbolC(name4), NumberC(4)]), 
//       ListC([SymbolC(name5), NumberC(5)]))
//                ...
//    RO: [[{pairName: Name(name2), pairExpr: parseExpression(NumberC(2))}],
//        [{pairName: Name(name3), pairExpr: parseExpression(NumberC(3))}],
//        [{pairName: Name(name4), pairExpr: parseExpression(NumberC(4))}],
//        [{pairName: Name(name5), pairExpr: parseExpression(NumberC(5))}]]

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

// OI: ListC(
//       [ListC([condExpr1, resultExpr1])]
//       [ListC([condExpr2, resultExpr2])]
//       [ListC([condExpr3, resultExpr3])]
//       [ListC([condExpr4, resultExpr4])]
//       [ListC([condExpr5, resultExpr5])])
//    RI: ListC(
//       [ListC([condExpr2, resultExpr2])]
//       [ListC([condExpr3, resultExpr3])]
//       [ListC([condExpr4, resultExpr4])]
//       [ListC([condExpr5, resultExpr5])])
//                ...
//    RO: [[{conditionExpr: boolExpr2,
//            resultExpr: resultExpr2,}],
//        [{conditionExpr: boolExpr3,
//            resultExpr: resultExpr3,}],
//        [{conditionExpr: boolExpr4,
//            resultExpr: resultExpr4,}],
//        [{conditionExpr: boolExpr5,
//            resultExpr: resultExpr5,}]]
// OO: [[{conditionExpr: boolExpr1,
//            resultExpr: resultExpr1,}],
//      [{conditionExpr: boolExpr2,
//            resultExpr: resultExpr2,}],
//      [{conditionExpr: boolExpr3,
//            resultExpr: resultExpr3,}],
//      [{conditionExpr: boolExpr4,
//            resultExpr: resultExpr4,}],
//      [{conditionExpr: boolExpr5,
//            resultExpr: resultExpr5,}]]

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

for parsing to an ApplicationE, where the first cpp dos not correspond to one 
    of the keywords, 
        OI: ListC([cpp1, cpp2, cpp3])
            RI: ListC([cpp2, cpp3])
              RI: ListC([cpp3])
              RO: ApplicationE([expr3])
            RO: ApplicationE([expr2, expr3])
        OO: ApplicationE([expr1, expr2, expr3])

        OI: ListC([cpp1, cpp2, cpp3, cpp4, cpp5])
            RI: ListC([cpp2, cpp3, cpp4, cpp5])
              ...
            RO: ApplicationE([expr2, expr3, expr4, expr5])
        OO: ApplicationE([expr1, expr2, expr3, expr4, expr5])
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
      | [SymbolC("and"), exp1, exp2] => 
        AndE(parseExpression(exp1), parseExpression(exp2))
      | [SymbolC("and"), ..._] => failwith("and: expected two arguments")
      | [SymbolC("or"), exp1, exp2] => 
        OrE(parseExpression(exp1), parseExpression(exp2))
      | [SymbolC("or"), ..._] => failwith("or: expected two arguments")
      | [SymbolC("lambda"), exp1, exp2] =>
        LambdaE({
          nameList: lambdaHelp(exp1),
          lambdaBody: parseExpression(exp2),
        })
      | [SymbolC("lambda"), ..._] =>
        failwith("lambda: expected two arguments")
      | [SymbolC("let"), exp1, exp2] =>
          LetE({letPairs: letHelp(exp1), letBody: parseExpression(exp2)})
      | [SymbolC("let"), ..._] => failwith("let: expected two arguments")
      | [SymbolC("cond"), ...tl] => CondE(condHelp(tl))
      | [SymbolC(x)] => NameE(Name(x))
      | [x] => parseExpression(x) /* short circuit for recursive calls */
      | [x, ...tl] =>
        ApplicationE(List.map(parseExpression, [x, ...tl]))
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

base case: empty env [] return None, as no name is binded to value in an empty
          environment. 

  OI: [(name1, value1), (name2, value2), (name3, value3)], targetName
     if name1 match targetName, return value1 in the format of Some(va); 
     if not match targetName, then 
     RI: [(name2, value2), (name3, value3)], targetName
        RI: [(name3, value3)], targetName
          RI: [] 
          if eventually reach [], return None.
          short circuit upon reaching an option of Some or None. 

  In case it would short circuit somewhere, e.g.: 
  OI: [(name1, value1), (name2, value2), (name3, value3)], targetName
     RI: [(name2, value2), (name3, value3)], targetName
        if name2 == targetName, then return 
      RO: Some(value2)
  OO: Some(value2)
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

/* procedure to convert let pairs into bindings and make the bindings into a 
  list, or an environment
  I/P: local environment, top level environment, a list of let pairs
  O/P: a list of bindings created from the let pairs, which is also an 
       environment

  OI: tle, local, [{pairName: name1, pairExpr: expr1},
      the envs are  {pairName: name2, pairExpr: expr2},
      same through  {pairName: name3, pairExpr: expr3}] 
      out the 
      procedure 
                RI: [{pairName: name2, pairExpr: expr2},
                      {pairName: name3, pairExpr: expr3}]]
                      RI: [{pairName: name3, pairExpr: expr3}]
                      RO: [(name3, value3)] (where value3 is the result of 
                                             evaluating expr3)
                RO: [(name2, value2), (name3, value3)]
  OO: [(name1, value1), (name2, value2), (name3, value3)]

  OI: tle, local, [{pairName: name1, pairExpr: expr1},
                    {pairName: name2, pairExpr: expr2},
                    {pairName: name3, pairExpr: expr3},
                    {pairName: name4, pairExpr: expr4},
                    {pairName: name5, pairExpr: expr5}]  
        RI: [{pairName: name2, pairExpr: expr2},
              {pairName: name3, pairExpr: expr3},
              {pairName: name4, pairExpr: expr4},
              {pairName: name5, pairExpr: expr5}]  
                ...
        RO: [(name2, value2), (name3, value3), (name4, value4), (name5, value5)]
  OO: [(name1, value1), (name2, value2), (name3, value3), 
        (name4, value4), (name5, value5)]
*/
let rec letPairHelper:
  (environment, environment, list(letPair)) => environment =
  (tle, local, lst) =>
    switch (lst) {
    | [] => []
    | [hd, ...tl] => 
        [(hd.pairName, eval(tle, local, hd.pairExpr)),
        ...letPairHelper(tle, local, tl)]
    }

//closureBindingsHelp: (environment, environment, list(name), 
//  list(expression)) => environment
//I/P: local environment, top level environment, an value
//O/P: the corresponding value after evaluation  

/*base case is both lists are empty
   
   the two [] in front indicate environments
   OI: ([],[], [Name("x"), Name("y")], [NumE(7), NumE(8)])
      RI: ([],[], [Name("y")], [NumE(8)])
        RI: ([],[],[],[])  
        RO: []
      RO: [(Name("x"), NumE(7))]
    OO: [(Name("x"), NumE(7)), (Name("y"), NumE(8))]
    
    OI: ([],[], [Name("x"), Name("y"), Name("z")], 
                [NumE(7), NumE(8), NumE(9)], )
      RI: ([],[], [Name("y"), Name("z")], [NumE(8), NumE(9)])
       ...
        RO: []
      RO: [(Name("y"), NumE(8)), (Name("z"), NumE(9))]
    OO: [(Name("x"), NumE(7)), (Name("y"), NumE(8)), (Name("z"), NumE(9))]*/

and closureBindingsHelp: (environment, environment, list(name), 
                          list(expression)) => environment =
  (tle, local, names, exps) =>
    switch(names, exps) {
    |([],[]) => []
    |([hd1,...tl1],[hd2,...tl2]) => [(hd1, eval(tle, local, hd2)),...
      closureBindingsHelp(tle, local, tl1, tl2)]
    |_ => failwith
          ("closureBindingsHelp: the given lists must be of equal length")
    }

/*eval: (environment, environment, expression) => value
 evaluate an expression with both the local and top level environment
  I/P: local environment, top level environment, an value
  O/P: the corresponding value after evaluation 
   Depending on the expression: 
    If one of the following: AndE, OrE, IfE, CondE,
      expression is evaluated based on their own respective rule of evaluation,
      and eval is called recursively upon their corresponding result expressions

    If ApplicationE: 
    Depending on whether builtin or user defined(closureV), 
    Builtin proc is applied upon the evaluated values from the arguments. 
    ClosureData is evaluated with the environment of the closure in addition to 
    the tle and local. 
  */

and eval: (environment, environment, expression) => value =
  (tle, local, expr) => {
    switch (expr) {
    | NumE(x) => NumV(x)
    | BoolE(boolExpr) => BoolV(boolExpr)
    | EmptyE => ListV([])
    | NameE(nom) =>
      switch (defLookUp((extendEnv(local, tle)), nom)) {
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
      | BuiltinV(x) => x.bProc(List.map(fun | expr => 
          eval(tle, local, expr), tl))
      | ClosureV(x) => eval(tle, 
                            extendEnv(extendEnv
                            (closureBindingsHelp(tle, 
                                                local, 
                                                x.cNameList, tl), x.cEnv), 
                                      local), 
                            x.cExpr)
      | _ =>
        failwith
          ("syntax error for builtin proc or closure")
      }
    |_ => failwith ("cond expr or app expr cannot contain empty list")
    };
  };
/* NOTE: tle is top level environment and env is local environment */

/* addDefinition: (environment, (name, expression)) => environment
   adding a definition, or a name expression, to an environment
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

/* stringOfValue: value => string
   procedure to convert the value from evaluation into a string
   I/P: a value
   O/P: the corresponding string representing the value

   base case is when only one value or a ListV([]) remains to be converted
   into strings. If ListV is not empty, keep going with recursive call. 
   
   OI: ListV([value1, value2, value3])
      RI: ListV([value2, value3])
        RI: ListV([value3])  
          RI: ListV([])
          RO: "empty"
        RO: "(cons string3 empty)" <=string3 is the stringOfValue of value3
      RO: "(cons string2 (cons string3 empty)"
    RO: "(cons string1 (cons string2 (cons string 3 empty)))"

   OI: ListV([value1, value2, value3, value4, value5])
      RI: ListV([value2, value3, value4, value5])
                  ...
      RO: "(cons string2 (cons string3 (cons string4 (cons string5 empty)" 
    RO: "(cons string1 
            (cons string2 (cons string 3 (cons string4 (cons string5 empty)"
*/
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

/* Process: abstractProgram => list(value) 
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

/* rackette: rawProgram => list(string)
  I/P: a raw racket program in a string format
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

//read parse true
checkExpectExpression(
  parseExpression(read("true")),
  BoolE(true),
  "read and parse true",
);

//read parse false
checkExpectExpression(
  parseExpression(read("false")),
  BoolE(false),
  "read and parse false",
);

//read parse a name 
checkExpectExpression(
  parseExpression(read("x")),
  NameE(Name("x")),
  "read and parse a name",
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

//read and parse for or expression
checkExpectAbstractProgram(
  parse([read ("(or false 5)")]),
[Expression(OrE(BoolE(false), NumE(5)))],
  "read and parse for and statement",
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
  "read and parse for let expression with two let pairs",
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

//parseExpression with read on buildin proc
checkExpectExpression(
  parseExpression(read("(+ 1 5)")),
  ApplicationE([NameE(Name("+")), NumE(1), NumE(5)]),
  "read and parse 1 + 5",
);

//parseExpression with read on buildin proc
checkExpectExpression(
  parseExpression(read("((lambda (x y) (+ x y)) 2 3 5)")),
  ApplicationE([LambdaE(
    {nameList: [Name("x"), Name("y")], 
    lambdaBody: ApplicationE([NameE(Name("+")), 
                              NameE(Name("x")), 
                              NameE(Name("y"))]),}),
     NumE(2), NumE(3), NumE(5)]),
  "lambda",
);
//parseExpression with read on buildin proc
checkExpectExpression(
  parseExpression(read("(2 3 5)")),
  ApplicationE([NumE(2), NumE(3), NumE(5)]),
  "lambda",
);


//string of value for buildin proc
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("(+ 1 5)")))),
  "6",
  "stringOfValue buildin proc +",
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

// eval parseExpression with read, single num
checkExpect(
  stringOfValue(eval(initialTle, [], parseExpression(read("1")))),
  "1",
  "read all the way for a num",
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

///more for buillt ins

//racekette for a number 
checkExpect(
  rackette("1"), ["1"], "rackette of a number"
  );

//racekette for a bool
checkExpect(
  rackette("true"), ["true"], "rackette of a bool"
  );

//racekette empty
checkExpect(
  rackette("empty"), ["empty"], "rackette empty"
  );

//racekette or
checkExpect(
  rackette("(or (= 3 5)(= 2 2))"), ["true"], "rackette or"
  );

//rackette and
checkExpect(
  rackette("(and true false)"),
  ["false"],
  "rackette and",
);

//rackette if
checkExpect(
  rackette("(if (zero? -5) 3 2)"),
  ["2"],
  "rackette if",
);

//rackette of cond expr
checkExpect(
  rackette("(cond ((= 7 8) 6) (true 4))"),["4"], "rackette of cond expr"
  );

//rackette for most builtin proc
checkExpect(
  rackette("(+ 1 5)(- 3 4)(* 8 9)(/ 8 4)(remainder 9 4)(< 7 8)(> 9 0)
  (<= 9 3)(>= 5 5)"), 
  ["6", "-1", "72", "2", "1", "true", "true", "false", "true"], 
  "rackette of builtin proc");

//rackette for most builtin proc 2
checkExpect(
  rackette("(and (equal? 7 8)(number? 90))"), 
  ["false"], "rackette of nested builtin proc bool");

//rackette for nested builtin proc
checkExpect(
  rackette("(+ 1 (- 3 (* 8 (/ 8 4))))"), 
  ["-12"], "rackette of nested builtin proc");

//rackette for cons of builtin proc
checkExpect(
  rackette("(cons (zero? 0)(cons (first (cons 2 empty)) empty))"), 
  ["(cons true (cons 2 empty))"], "rackette of cons of nested builtin proc");

//rackette of builtin proc
checkExpect(
  rackette("(empty? (rest (cons 8 ( cons true empty))))"), 
  ["false"], "rackette of nested builtin proc 2");

//rackette of def and application exp
checkExpect(
  rackette("(define nineLengthen (lambda (n alon)
  (if (= n 9) (cons n alon) alon))) (nineLengthen 9 empty)"), 
  ["(cons 9 empty)"], "rackette of def and application exp");

//rackette of def and application exp
checkExpect(
  rackette("(define nineLengthen (lambda (n alon)
  (if (= n 9) (cons n alon) alon))) 
  (let ((x 8)(y 0)) (nineLengthen x (cons 8 (cons y empty))))"), 
  ["(cons 8 (cons 0 empty))"], "rackette of def and application exp");

//rackette of user-defined
checkExpect(
  rackette("(lambda (n alon) (nineLengthen x (cons 8 (cons y empty))))"), 
  ["user-defined procedure"], "rackette of user-defined");

//rackette of nested lambda
checkExpect(
  rackette("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"), 
  ["34"], "rackette of nested lambda");


//rackette of not
checkExpect(
  rackette("(not (not (not true)))"), 
  ["false"], "rackette of not");


//test cases for all error

//"invalid input addition"
//rackette
checkError(() => rackette("(+ 3 false)"), "invalid input addition");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(+ 3 false)"))), "invalid input addition");
//rackette
checkError(() => rackette("(+ 3 false 4)"), "invalid input addition"); 
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(+ 3 false 4)"))), "invalid input addition");

//"invalid input subtraction"
//rackette
checkError(() => rackette("(- 3 4 5)"), "invalid input subtraction");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(- 3 4 5)"))), "invalid input subtraction");

//"invalid input multiplication"
//rackette
checkError(() => rackette("(* true false)"), "invalid input multiplication");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(* true false)"))), "invalid input multiplication");
  
//"invalid input division"
//rackette
checkError(() => rackette("(/ true false)"), "invalid input division");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(/ true false)"))), "invalid input division");

//"invalid input num equal"
//rackette
checkError(() => rackette("(= 3)"), "invalid input num equal");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(= 3)"))), "invalid input num equal");

//"invalid input greater than"
//rackette
checkError(() => rackette("(> 1)"), "invalid input greater than");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(> 3)"))), "invalid input greater than");

//"invalid input less than"
//rackette
checkError(() => rackette("(< 1 true)"), "invalid input less than");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(< 1 true)"))), "invalid input less than");

//"invalid input greater than or equal to"
//rackette
checkError(() => rackette("(>= 1)"), "invalid input greater than or equal to");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(>= 1)"))), "invalid input greater than or equal to");

// "invalid input less than or equal to"
//rackette
checkError(() => rackette("(<= 1)"), "invalid input less than or equal to");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(<= 1)"))), "invalid input less than or equal to");

//"invalid input equality"
//rackette
checkError(() => rackette("(equal? 1 2 3)"), 
  "invalid input equality");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(equal? 1 2 3)"))), "invalid input equality");

//"invalid input isnum must be one argument"
//rackette
checkError(() => rackette("(number? 1 2)"), 
  "invalid input isnum must be one argument");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(number? 1 2)"))), 
  "invalid input isnum must be one argument");

//"invalid input zerom must be one argument num"
//rackette
checkError(() => rackette("(zero? 1 2)"), 
  "invalid input is input zero, must be one argument num");
checkError(() => rackette("(zero? true)"), 
  "invalid input is input zero, must be one argument num");
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(zero? 1 2)"))), 
  "invalid input is input zero, must be one argument num");
  checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(zero? true)"))), 
  "invalid input is input zero, must be one argument num");

//"invalid input expect two arguments"
//rackette
checkError(() => rackette("(cons 1)"), 
  "invalid input expect two arguments"); 
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(cons 1)"))), 
  "invalid input expect two arguments");

//"invalid input second item must be list"
//rackette
checkError(() => rackette("(cons 1 2)"), 
  "invalid input second item must be list");
//eval  
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(cons 1 2)"))), 
  "invalid input second item must be list");

// "invalid input first must be a list"
//rackette
checkError(() => rackette("(first 1)"), 
  "invalid input first must be a list"); 
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(first 1)"))), 
  "invalid input first must be a list");

// "invalid input rest must be a list"
//rackette
checkError(() => rackette("(rest 1)"), 
  "invalid input rest must be a list");  
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(rest 1)"))), 
  "invalid input rest must be a list");  

// "invalid input empty? must be a list"
//rackette
checkError(() => rackette("(empty? 5)"), 
  "invalid input empty? must be a list");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(empty? 5)"))), 
  "invalid input empty? must be a list");  

// "invalid input cons? must be a list"
//rackette
checkError(() => rackette("(cons? 2)"), 
  "invalid input cons? must be a list"); 
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(cons? 2)"))), 
  "invalid input cons? must be a list");  

//"invalid input not must be a boolean"
//rackette
checkError(() => rackette("(not 2)"), 
  "invalid input not must be a boolean");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(not 2)"))), 
  "invalid input not must be a boolean"); 

//"rackette cannot handle an empty list"
//rackette
checkError(() => rackette("()"), "rackette cannot handle an empty list");

//"invalid input lambdaHelp"
//rackette
checkError(() => rackette("(lambda x y)"), "invalid input lambdaHelp");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(lambda x y)"))), 
  "invalid input lambdaHelp");
//parse
checkError(() =>  
  parseExpression(read("(lambda x y)")), 
  "invalid input lambdaHelp");

//"invalid input letHelp needs ListC(SymbolC(string)...)"
//rackette
checkError(
  () => rackette("(let (4 x) 4)"),
  "invalid input letHelp needs ListC(SymbolC(string)...)",
);
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(let (4 x) 4)"))), 
  "invalid input letHelp needs ListC(SymbolC(string)...)");
//parse
checkError(() => 
  parseExpression(read("(let (4 x) 4)")), 
  "invalid input letHelp needs ListC(SymbolC(string)...)");


//"letHelp must take in a list of concrete program pieces"
//rackette
checkError(
  () => rackette("(let x y)"),
  "letHelp must take in a list of concrete program pieces",
);
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(let x y)"))), 
  "letHelp must take in a list of concrete program pieces");
//parse 
checkError(() => 
  parseExpression(read("(let x y)")), 
  "letHelp must take in a list of concrete program pieces");

//"let: expected two arguments" 
//rackette
checkError(() => rackette("(let (x y))"), "let: expected two arguments");
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(let (x y))"))), 
  "let: expected two arguments");
//parse
checkError(() => 
  parseExpression(read("(let (x y))")), 
  "let: expected two arguments");

// "cond needs a list of bool, expr pairs"
//rackette
checkError(
  () => rackette("(cond)"),
  "cond needs a list of bool, expr pairs",
);
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(cond)"))), 
  "cond needs a list of bool, expr pairs");
//parse
checkError(() => 
  parseExpression(read("(cond)")), 
  "cond needs a list of bool, expr pairs");

//"and: expected two arguments"
//rackette
checkError(() => rackette("(and true)"), "and: expected two arguments");
checkError(() => rackette("(and)"), "and: expected two arguments");
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(and true)"))), 
  "and: expected two arguments");
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(and)"))), 
  "and: expected two arguments");
//parse
checkError(() => 
  parseExpression(read("(and true)")), 
  "and: expected two arguments");
checkError(() => 
  parseExpression(read("(and)")), 
  "and: expected two arguments");

//"and expects two bool expr"  
//rackette
checkError(() => rackette("(and 5 true)"), "and expects two bool expr");
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(and 5 true)"))), 
  "and expects two bool expr");

//"or: expected two arguments"
//rackette
checkError(() => rackette("(or 5)"), "or: expected two arguments");
//eval 
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(or 5)"))), 
  "or: expected two arguments");
//parse
checkError(() => 
  parseExpression(read("(or 5)")), 
  "or: expected two arguments");

//"or expects two bool expr"
//rackette
checkError(() => rackette("(or 5 6)"), "or expects two bool expr");
//eval
checkError(() => 
  eval((initialTle), [], 
  parseExpression(read("(or 5 6)"))), 
  "or expects two bool expr");

//lambda: expected two arguments
checkError(() => rackette("(lambda 5)"), 
                  "lambda: expected two arguments");
checkError(() => parse(readAll("(lambda 5)")), 
                  "lambda: expected two arguments");
checkError(() => rackette("(lambda)"), 
                  "lambda: expected two arguments");
checkError(() => parse([ListC([SymbolC("lambda")])]), 
                  "lambda: expected two arguments");

//let:expected two arguments
checkError(() => rackette("(let 5)"), "let: expected two arguments");
checkError(() => parse([ListC([SymbolC("let"), NumberC(5)])]), 
                  "let: expected two arguments");
checkError(() => rackette("(let)"), "let: expected two arguments");
checkError(() => parse(readAll("(let)")), "let: expected two arguments");

//define expects variable name followed by expression: incorrect format
checkError(
  () => rackette("(define 6 7)"),
  "define expects variable name followed by expression: incorrect format",
);

checkError(
  () => parse(readAll("(define 6 7)")),
  "define expects variable name followed by expression: incorrect format",
);

checkError(
  () => parseDefinition(ListC([SymbolC("define"), NumberC(6), NumberC(7)])),
  "define expects variable name followed by expression: incorrect format",
);

//if-expr must eval to a bool
checkError(() => rackette("(if 9 9 0)"), "if-expr must eval to bool");
checkError(() => eval(initialTle, [], 
                      IfE({boolExpr:NumE(9), 
                          trueExpr:NumE(9), 
                          falseExpr:NumE(0)})), 
                  "if-expr must eval to bool");


//expected three arguments
checkError(() => rackette("(if true)"), "expected three arguments");
checkError(() => parse([ListC([SymbolC("if"), SymbolC("true")])]), 
                  "expected three arguments");

//condition expr of cond must eval to bool
checkError(
  () => eval(initialTle, [], 
          CondE([ {conditionExpr: BoolE(false), resultExpr: NumE(6)}, 
                  {conditionExpr:NumE(8), resultExpr: NumE(7)}])),
  "condition expr of cond must eval to bool",
);

checkError(
  () => rackette("(cond (false 6) (8 7))"),
  "condition expr of cond must eval to bool",
);

//name already bound to value
checkError(
  () => rackette("(define x 7)(define x 8)"),
  "name already bound to value",
);

checkError(
  () => process(parse(readAll("(define x 7)(define x 8)"))),
  "name already bound to value",
);

//syntax error for builtin proc or closure
checkError(
  () => rackette("(3 4 5)"),
  "syntax error for builtin proc or closure",
);

checkError(
  () => eval(initialTle,[], ApplicationE([NumE(3), NumE(4), NumE(5)])),
  "syntax error for builtin proc or closure",
);

//closureBindingsHelp: the given lists must be of equal length
checkError(
  () => rackette("((lambda (n alon) (cons n alon)) 3 4 empty)"),
  "closureBindingsHelp: the given lists must be of equal length",
);
checkError(
  () => eval(initialTle, [], 
  ApplicationE([LambdaE({nameList: [Name("n"), Name("alon")], 
                        lambdaBody: ApplicationE([NameE(Name("cons")), 
                                                NameE(Name("n")), 
                                                NameE(Name("alon"))])}), 
                NumE(3), NumE(4), EmptyE])),
  "closureBindingsHelp: the given lists must be of equal length");
checkError(
  () => closureBindingsHelp(initialTle,[], 
        [Name("x"), Name("y")], [NumE(3), NumE(4), EmptyE]),
  "closureBindingsHelp: the given lists must be of equal length");
 
