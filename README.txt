Banner IDs: B01784729 B01857919

A user would input a rawProgram which is a string that represents their rackette 
program. It could include expressions and definitions. The output of their raw 
program in Rackette would be a list of strings that represent the value of the 
evaluated expressions within the input. For example if the use were to input: 
“(define f (lambda (x ) (* x 5))) (f 7) (f 8)”
Rackette would output:
[“35”, “45”]
Rackette consists of 4 main helper procedures: read, parse, eval, and process. 
When a rawProgram is sent into rackette, it is first read. When it is read, the 
rawProgram is translated into a concreteProgram which is a series of 
concreteProgramPieces, which are either NumberC(int), SymbolC(string), or 
ListC(list(concreteProgramPiece). 

Then, the output of read is sent into parse which converts the concreteProgram 
into an abstractProgram which is a list of concreteProgramPieces, either 
Definition(definition) or Expression(expression). Parse uses helpers 
parseExpression and parseDefinition to differentiate the given 
concreteProgramPieces into either category. When dealing with 
ListC([SymbolC(“define”)...]), parseDefinition is called. The following name and 
parsed expression are made into a pair. Anything else is sent into 
PareExpression. Depending on the syntax of the input of type 
concreteProgramPiece, parseExpression translates the input into a list of 
expressions where an expression is either NumE(int), BoolE(bool), EmptyE, 
NameE(name), AndE(expression,expression), OrE(expression, expression), 
IfE(ifData), CondE(list(condData)), LambdaE(lambdaData), LetE(letData), or 
ApplicationE(list(expression)). The created expressions or definitions are 
wrapped into Expression() or Definition() wrappers respectively. The output of 
parse, an abstractProgram is then sent into process as an argument.

In process, the abstractProgram is translated into a list(value) where values 
are either NumV(int), BoolV(int), ListV(list(value)), BuiltinV(builtInData), 
or ClosureV(closureData). It uses a helper processHelper that operates 
recursively to add definitions to the top level environment in bindings 
of (name, expression) and evaluate expressions to their corresponding value 
using eval. The output of process is sent into string_of_value which simply 
converts the list(value) into list(string) to then be output by rackette. 

Possible Bugs: N/A

 Collaboration partners: Simone Dunbar, Dianna Pei

