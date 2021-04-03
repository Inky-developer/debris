initSidebarItems({"enum":[["HirComparisonOperator","Any supported comparison operator"],["HirConstValue","A constant literal, already parsed"],["HirControlKind",""],["HirDeclarationMode",""],["HirExpression","Any expression"],["HirInfixOperator","Any operator that can be used as an infix"],["HirItem","Any Item"],["HirObject",""],["HirPrefixOperator","Any prefix operator"],["HirStatement","Any statement, the difference to an expression is that a statement does not return anything"],["HirTypePattern","Any pattern that is allowed to specify a function parameter type"]],"struct":[["Attribute","Attributes are a form of metadata that can be applied to any object."],["HirBlock","A block of code. Usually contained withing a pair of {} parenthesis."],["HirConditionalBranch","An if-branch which checks a condition and runs code depending on whether the condition is true or not"],["HirControlFlow","Represents a control flow statement like return or break"],["HirFunction","A function, which contains other statements"],["HirFunctionCall","Any function call, can be dotted"],["HirImport","Marks an import statement. The id specifies the index of the matching [HirModule]"],["HirInfiniteLoop","An infinite loop (Can be exited using control keywords like `break` and `return`)"],["HirInfix","Holds an infix operator combined with its span"],["HirModule","A module with an associated name"],["HirParameterDeclaration","Holds a variable type declaration like `foo: String` This is used in method signatures"],["HirPrefix","Holds a prefix operator combined with its span"],["HirPropertyDeclaration","Declaration of a property in a struct definition"],["HirStruct","A struct definition"],["HirVariableInitialization","Sets a variable like `let a = expression();`"],["HirVariableUpdate","Similar to `HirVariableInitialization`, however this node marks a write to an already initialized value"]]});