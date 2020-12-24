(function() {var implementors = {};
implementors["debris_backends"] = [{"text":"impl Unpin for MinecraftCommand","synthetic":true,"types":[]},{"text":"impl Unpin for ExecuteComponent","synthetic":true,"types":[]},{"text":"impl Unpin for MinecraftRange","synthetic":true,"types":[]},{"text":"impl Unpin for ScoreboardPlayer","synthetic":true,"types":[]},{"text":"impl Unpin for ObjectiveCriterion","synthetic":true,"types":[]},{"text":"impl Unpin for FunctionIdent","synthetic":true,"types":[]},{"text":"impl Unpin for DatapackBackend","synthetic":true,"types":[]}];
implementors["debris_common"] = [{"text":"impl Unpin for Ident","synthetic":true,"types":[]},{"text":"impl Unpin for SpecialIdent","synthetic":true,"types":[]},{"text":"impl Unpin for Accessor","synthetic":true,"types":[]},{"text":"impl Unpin for InputFile","synthetic":true,"types":[]},{"text":"impl Unpin for Code","synthetic":true,"types":[]},{"text":"impl Unpin for LocalSpan","synthetic":true,"types":[]},{"text":"impl Unpin for Span","synthetic":true,"types":[]}];
implementors["debris_core"] = [{"text":"impl Unpin for Hir","synthetic":true,"types":[]},{"text":"impl Unpin for HirConstValue","synthetic":true,"types":[]},{"text":"impl Unpin for HirComparisonOperator","synthetic":true,"types":[]},{"text":"impl Unpin for HirInfixOperator","synthetic":true,"types":[]},{"text":"impl Unpin for HirInfix","synthetic":true,"types":[]},{"text":"impl Unpin for HirPrefixOperator","synthetic":true,"types":[]},{"text":"impl Unpin for HirPrefix","synthetic":true,"types":[]},{"text":"impl Unpin for HirVariableDeclaration","synthetic":true,"types":[]},{"text":"impl Unpin for HirPropertyDeclaration","synthetic":true,"types":[]},{"text":"impl Unpin for HirFunctionCall","synthetic":true,"types":[]},{"text":"impl Unpin for HirExpression","synthetic":true,"types":[]},{"text":"impl Unpin for HirStatement","synthetic":true,"types":[]},{"text":"impl Unpin for HirFunction","synthetic":true,"types":[]},{"text":"impl Unpin for HirBlock","synthetic":true,"types":[]},{"text":"impl Unpin for HirStruct","synthetic":true,"types":[]},{"text":"impl Unpin for HirObject","synthetic":true,"types":[]},{"text":"impl Unpin for SpannedIdentifier","synthetic":true,"types":[]},{"text":"impl Unpin for IdentifierPath","synthetic":true,"types":[]},{"text":"impl Unpin for DebrisParser","synthetic":true,"types":[]},{"text":"impl Unpin for Rule","synthetic":true,"types":[]},{"text":"impl Unpin for Llir","synthetic":true,"types":[]},{"text":"impl Unpin for Function","synthetic":true,"types":[]},{"text":"impl Unpin for FastStore","synthetic":true,"types":[]},{"text":"impl Unpin for FastStoreFromResult","synthetic":true,"types":[]},{"text":"impl Unpin for BinaryOperation","synthetic":true,"types":[]},{"text":"impl Unpin for Call","synthetic":true,"types":[]},{"text":"impl Unpin for Condition","synthetic":true,"types":[]},{"text":"impl Unpin for Branch","synthetic":true,"types":[]},{"text":"impl Unpin for Execute","synthetic":true,"types":[]},{"text":"impl Unpin for Node","synthetic":true,"types":[]},{"text":"impl Unpin for Scoreboard","synthetic":true,"types":[]},{"text":"impl Unpin for ScoreboardValue","synthetic":true,"types":[]},{"text":"impl Unpin for ScoreboardOperation","synthetic":true,"types":[]},{"text":"impl Unpin for ScoreboardComparison","synthetic":true,"types":[]},{"text":"impl Unpin for ItemId","synthetic":true,"types":[]},{"text":"impl Unpin for ItemIdentifier","synthetic":true,"types":[]},{"text":"impl Unpin for MirValue","synthetic":true,"types":[]},{"text":"impl Unpin for MirNode","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Unpin for MirInfo&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Unpin for MirContextInfo&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for MirNamespaceEntry","synthetic":true,"types":[]},{"text":"impl Unpin for MirContext","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Unpin for MirBuilder&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for Mir","synthetic":true,"types":[]},{"text":"impl Unpin for CompileContext","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Unpin for Namespace&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Unpin,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Unpin for ObjectRef","synthetic":true,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized&gt; Unpin for DebrisObject&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Unpin,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Unpin for ObjBool","synthetic":true,"types":[]},{"text":"impl Unpin for ObjStaticBool","synthetic":true,"types":[]},{"text":"impl Unpin for ObjClass","synthetic":true,"types":[]},{"text":"impl Unpin for ObjFunction","synthetic":true,"types":[]},{"text":"impl Unpin for CallbackFunction","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Unpin for FunctionContext&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for FunctionSignature","synthetic":true,"types":[]},{"text":"impl Unpin for ObjStaticInt","synthetic":true,"types":[]},{"text":"impl Unpin for ObjInt","synthetic":true,"types":[]},{"text":"impl Unpin for ObjModule","synthetic":true,"types":[]},{"text":"impl Unpin for ModuleFactory","synthetic":true,"types":[]},{"text":"impl Unpin for ObjString","synthetic":true,"types":[]},{"text":"impl Unpin for BuildMode","synthetic":true,"types":[]},{"text":"impl Unpin for Config","synthetic":true,"types":[]},{"text":"impl Unpin for CompileError","synthetic":true,"types":[]},{"text":"impl Unpin for ParseError","synthetic":true,"types":[]},{"text":"impl Unpin for LangError","synthetic":true,"types":[]},{"text":"impl Unpin for LangErrorKind","synthetic":true,"types":[]},{"text":"impl Unpin for Type","synthetic":true,"types":[]}];
implementors["debris_lang"] = [{"text":"impl Unpin for CompileConfig","synthetic":true,"types":[]}];
implementors["vfs"] = [{"text":"impl&lt;'a&gt; Unpin for FsElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Unpin for File","synthetic":true,"types":[]},{"text":"impl Unpin for Directory","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()