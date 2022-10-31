type rawProgram = string;

type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/* 
NumberC(int): NumberC(12),  NumberC(8),  NumberC(0),  NumberC(-2)
SymbolC(string): SymbolC("+"), SymbolC("-"), SymbolC("/") 
ListC(list(concreteProgramPiece)):
ListC(list(NumberC(12), SymbolC("-"), NumberC(9), SymbolC("+")))
ListC(list(SymbolC("/"), NumberC(11), NumberC(15)))
ListC(list(SymbolC("*"), NumberC(0), NumberC(9378)))
*/

type concreteProgram = list(concreteProgramPiece);
/*
list(ListC(list(SymbolC("*"), NumberC(0), NumberC(9378))), NumberC(12), NumberC(8))
list(NumberC(12), SymbolC("+"), SymbolC())
*/

/* a Rackette name 
Name("x") 
Name("sequence")
Name("variablename")
*/
type name =
  | Name(string);

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
  
/* a Rackette definition 
(Name("x"), NumE(18))
(Name("alod"), EmptyE)
(Name("application"), ApplicationE(list(expression)))
*/
type definition = (name, expression);

/* a piece of Rackette that can be processed:
 * either a definition or an expression */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* a representation of a Rackette program -
 * any number of pieces */
type abstractProgram = list(abstractProgramPiece);

/* a Rackette value: the result of evaluating a Rackette expression */
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
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