// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

function printGreen(s) {
  console.log("\x1b[32m" + (s + "\x1b[0m"));
  
}

function printRed(s) {
  console.log("\x1b[31m" + (s + "\x1b[0m"));
  
}

function stringOfAList(lst, strOf) {
  return "[" + List.fold_right((function (a, b) {
                return Curry._1(strOf, a) + ((
                          b === "]" ? "" : ", "
                        ) + b);
              }), lst, "]");
}

function stringOfConcreteProgramPiece(concrpiece) {
  switch (concrpiece.TAG | 0) {
    case /* NumberC */0 :
        return "NumberC(" + (String(concrpiece._0) + ")");
    case /* SymbolC */1 :
        return "SymbolC(" + (concrpiece._0 + ")");
    case /* ListC */2 :
        return "ListC(" + (stringOfAList(concrpiece._0, stringOfConcreteProgramPiece) + ")");
    
  }
}

function stringOfConcreteProgram(concrprog) {
  return stringOfAList(concrprog, stringOfConcreteProgramPiece);
}

function stringOfName(namex) {
  return "Name(" + (namex._0 + ")");
}

function stringOfNameList(nameList) {
  return stringOfAList(nameList, stringOfName);
}

function stringOfExpression(expr) {
  if (typeof expr === "number") {
    return "EmptyE";
  }
  switch (expr.TAG | 0) {
    case /* NumE */0 :
        return "NumE(" + (String(expr._0) + ")");
    case /* BoolE */1 :
        return "BoolE(" + (Pervasives.string_of_bool(expr._0) + ")");
    case /* NameE */2 :
        return "NameE(" + (stringOfName(expr._0) + ")");
    case /* AndE */3 :
        return "AndE(" + (stringOfExpression(expr._0) + (", " + (stringOfExpression(expr._1) + ")")));
    case /* OrE */4 :
        return "OrE(" + (stringOfExpression(expr._0) + (", " + (stringOfExpression(expr._1) + ")")));
    case /* IfE */5 :
        throw {
              RE_EXN_ID: "Match_failure",
              _1: [
                "CS17SetupRackettePREDESIGNCHECK.re",
                77,
                4
              ],
              Error: new Error()
            };
    case /* CondE */6 :
        var stringOfCondRecord = function (condD) {
          return "{conditionExpr: " + (stringOfExpression(condD.conditionExpr) + (", resultExpr: " + (stringOfExpression(condD.resultExpr) + ",}")));
        };
        return "CondE(" + (stringOfAList(expr._0, stringOfCondRecord) + ")");
    case /* LambdaE */7 :
        var lambdaD = expr._0;
        return "LambdaE({nameList: " + (stringOfAList(lambdaD.nameList, stringOfName) + (", lambdaBody: " + (stringOfExpression(lambdaD.lambdaBody) + ",})")));
    case /* LetE */8 :
        var letD = expr._0;
        var stringOfLetExpressionPair = function (pair) {
          return "{pairName: " + (stringOfName(pair.pairName) + (", pairExpr: " + (stringOfExpression(pair.pairExpr) + "}")));
        };
        return "LetE({" + (stringOfAList(letD.letPairs, stringOfLetExpressionPair) + (", letBody: " + (stringOfExpression(letD.letBody) + ",})")));
    case /* ApplicationE */9 :
        return "ApplicationE({" + (stringOfAList(expr._0, stringOfExpression) + "})");
    
  }
}

function stringOfDefinition(def) {
  return "(" + (stringOfName(def[0]) + (", " + (stringOfExpression(def[1]) + ")")));
}

function stringOfAbstractProgramPiece(piece) {
  if (piece.TAG === /* Definition */0) {
    return "Definition(" + (stringOfDefinition(piece._0) + ")");
  } else {
    return "Expression(" + (stringOfExpression(piece._0) + ")");
  }
}

function stringOfAbstractProgram(abstr) {
  return stringOfAList(abstr, stringOfAbstractProgramPiece);
}

function checkExpect(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ce_Success: " + message);
  } else {
    printRed("ce_Fail: " + message);
    printRed("expected output: ");
    console.log(expected);
    printRed("actual output: ");
    console.log(actual);
    return ;
  }
}

function checkExpectConcreteProgramPiece(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfConcreteProgramPiece(expected));
    printRed("actual output: ");
    return printRed(stringOfConcreteProgramPiece(actual));
  }
}

function checkExpectConcreteProgram(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfAList(expected, stringOfConcreteProgramPiece));
    printRed("actual output: ");
    return printRed(stringOfAList(actual, stringOfConcreteProgramPiece));
  }
}

function checkExpectName(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfName(expected));
    printRed("actual output: ");
    return printRed(stringOfName(actual));
  }
}

function checkExpectExpression(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfExpression(expected));
    printRed("actual output: ");
    return printRed(stringOfExpression(actual));
  }
}

function checkExpectDefinition(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfDefinition(expected));
    printRed("actual output: ");
    return printRed(stringOfDefinition(actual));
  }
}

function checkExpectAbstractProgramPiece(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfAbstractProgramPiece(expected));
    printRed("actual output: ");
    return printRed(stringOfAbstractProgramPiece(actual));
  }
}

function checkExpectAbstractProgram(actual, expected, message) {
  if (Caml_obj.caml_equal(actual, expected)) {
    return printGreen("ceSuccess: " + message);
  } else {
    printRed("ceFail: " + message);
    printRed("expected output: ");
    printRed(stringOfAList(expected, stringOfAbstractProgramPiece));
    printRed("actual output: ");
    return printRed(stringOfAList(actual, stringOfAbstractProgramPiece));
  }
}

function checkWithin(actual, expected, within) {
  if (Math.abs(actual - expected) <= Math.abs(within)) {
    return printGreen("cwSuccess ");
  } else {
    printRed("cwFail ");
    printRed("expected output: ");
    console.log(expected);
    printRed("actual output: ");
    console.log(actual);
    return ;
  }
}

function checkError(thunk, expect) {
  try {
    Curry._1(thunk, undefined);
    return Pervasives.failwith("Error did not occur");
  }
  catch (raw_err){
    var err = Caml_js_exceptions.internalToOCamlException(raw_err);
    if (err.RE_EXN_ID === "Failure") {
      var err$1 = err._1;
      if (err$1 === expect) {
        return printGreen("checkErrorSuccess ");
      } else if (err$1 === "Error did not occur") {
        return printRed("Error did not occur");
      } else {
        return printRed("checkErrorFail. Expected error: " + (expect + ("; Actual error: " + err$1)));
      }
    }
    throw err;
  }
}

exports.printGreen = printGreen;
exports.printRed = printRed;
exports.stringOfAList = stringOfAList;
exports.stringOfConcreteProgramPiece = stringOfConcreteProgramPiece;
exports.stringOfConcreteProgram = stringOfConcreteProgram;
exports.stringOfName = stringOfName;
exports.stringOfNameList = stringOfNameList;
exports.stringOfExpression = stringOfExpression;
exports.stringOfDefinition = stringOfDefinition;
exports.stringOfAbstractProgramPiece = stringOfAbstractProgramPiece;
exports.stringOfAbstractProgram = stringOfAbstractProgram;
exports.checkExpect = checkExpect;
exports.checkExpectConcreteProgramPiece = checkExpectConcreteProgramPiece;
exports.checkExpectConcreteProgram = checkExpectConcreteProgram;
exports.checkExpectName = checkExpectName;
exports.checkExpectExpression = checkExpectExpression;
exports.checkExpectDefinition = checkExpectDefinition;
exports.checkExpectAbstractProgramPiece = checkExpectAbstractProgramPiece;
exports.checkExpectAbstractProgram = checkExpectAbstractProgram;
exports.checkWithin = checkWithin;
exports.checkError = checkError;
/* No side effect */
