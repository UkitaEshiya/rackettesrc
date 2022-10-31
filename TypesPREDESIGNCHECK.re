type rawProgram = string;


type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* 
data examples:
NumberC(int): NumberC(12),  NumberC(8),  NumberC(0),  NumberC(-2)
SymbolC(string): SymbolC("+"), SymbolC("-"), SymbolC("/") 
ListC(list(concreteProgramPiece)):
ListC(list(NumberC(12), SymbolC("-"), NumberC(9), SymbolC("+")))
ListC(list(SymbolC("/"), NumberC(11), NumberC(15)))
ListC(list(SymbolC("*"), NumberC(0), NumberC(9378)))
*/

type concreteProgram = list(concreteProgramPiece);
/*
data examples:
list(ListC(list(SymbolC("*"), NumberC(0), NumberC(9378))), NumberC(12), NumberC(8))
list(NumberC(12), SymbolC("+"), SymbolC())
*/

// a Rackette name 

type name =
  | Name(string);

/*Name("x") 
Name("sequence")
Name("variablename")
*/

/* a Rackette expression */
type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData)) 
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))
  and ifData = {
    ifExpr: expression,
    yesExpr: expression, 
    noExpr: expression,
  }
  and condData = { 
    conditionExpr: expression, 
    resultExpr: expression,
  }
  and lambdaData = {
    nameList: list(name),
    lambdaBody: expression,
  }
  and letPair = {
    pairName: name, 
    pairExpr: expression,  
  }
  and letData = {
    letPairs: list(letPair),
    letBody: expression,
  }

/* data examples:
NumE(890)
BoolE(false)
EmptyE
NameE(Name("x"))
AndE(OrE(BoolE(true), BoolE(false)), BoolE(true))
OrE(BoolE(false), BoolE(false))
IfE({ApplicationE(list(NameE(Name("=")), NumE(5), NameE(Name("x"))), BoolE(true), BoolE(false))})CondE(list(condData)) 
LambdaE(list(Name("x")), ApplicationE(list(NameE(Name("+")), NameE(Name("x")), NumE(7))))
LetE({list(Name("x"),NumE(78)),OrE(BoolE(false), BoolE(false))})
ApplicationE(list(NameE(Name("+")), NameE(Name("x")), NumE(7)))
*/
  
// a Rackette definition 
type definition = (name, expression);
/*(Name("x"), NumE(18))
(Name("alod"), EmptyE)
(Name("application"), ApplicationE(list(expression)))
*/

/* a piece of Rackette that can be processed:
 * either a definition or an expression */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);
/* data examples:
Definition((Name("x"), NumE(18)))
Definition((Name("alod"), ApplicationE(list(NumE(2), NumE(4)))))
Expression(NumE(7))
Expression(BoolE(true))
*/

/* a representation of a Rackette program -
 * any number of pieces */
type abstractProgram = list(abstractProgramPiece);
/* data examples:
list(Definition((Name("x"), NumE(18))), Definition((Name("y"), NumE(30)))))
list(Expression(BoolE(true)))
*/

/* a Rackette value: the result of evaluating a Rackette expression */
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)  // user defined
  and builtinData = { 
    printedRep: string,
    bProc: list(value) => value,
  }
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }
  /* Environments and bindings aren't values
     But we use "and" here so bindings have access to values
     and closures have access to environments */
  and environment = (list(binding))
  and binding = (name, value);

  /* data examples:
  NumV(15)
  BoolV(true)
  ListV(list(NumV(9), NumV(78), NumV(80)))
  BuiltinV({printedRep: "builtin:+", bProc: plus,})
  ClosureV({list(Name("y")), NumE(45),(list(Name("x"), NumV("34"))})
  */

