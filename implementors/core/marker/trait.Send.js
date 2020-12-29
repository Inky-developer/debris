(function() {var implementors = {};
implementors["debris_backends"] = [{"text":"impl !Send for MinecraftCommand","synthetic":true,"types":[]},{"text":"impl !Send for ExecuteComponent","synthetic":true,"types":[]},{"text":"impl Send for MinecraftRange","synthetic":true,"types":[]},{"text":"impl !Send for ScoreboardPlayer","synthetic":true,"types":[]},{"text":"impl Send for ObjectiveCriterion","synthetic":true,"types":[]},{"text":"impl !Send for FunctionIdent","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for DatapackBackend&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["debris_common"] = [{"text":"impl Send for Ident","synthetic":true,"types":[]},{"text":"impl Send for SpecialIdent","synthetic":true,"types":[]},{"text":"impl Send for Accessor","synthetic":true,"types":[]},{"text":"impl Send for Code","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for CodeRef&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for InputFiles","synthetic":true,"types":[]},{"text":"impl Send for Span","synthetic":true,"types":[]}];
implementors["debris_core"] = [{"text":"impl&lt;'code&gt; Send for Hir&lt;'code&gt;","synthetic":true,"types":[]},{"text":"impl Send for HirConstValue","synthetic":true,"types":[]},{"text":"impl Send for HirComparisonOperator","synthetic":true,"types":[]},{"text":"impl Send for HirInfixOperator","synthetic":true,"types":[]},{"text":"impl Send for HirInfix","synthetic":true,"types":[]},{"text":"impl Send for HirPrefixOperator","synthetic":true,"types":[]},{"text":"impl Send for HirPrefix","synthetic":true,"types":[]},{"text":"impl Send for HirVariableDeclaration","synthetic":true,"types":[]},{"text":"impl Send for HirPropertyDeclaration","synthetic":true,"types":[]},{"text":"impl Send for HirFunctionCall","synthetic":true,"types":[]},{"text":"impl Send for HirExpression","synthetic":true,"types":[]},{"text":"impl Send for HirStatement","synthetic":true,"types":[]},{"text":"impl Send for HirFunction","synthetic":true,"types":[]},{"text":"impl Send for HirBlock","synthetic":true,"types":[]},{"text":"impl Send for HirStruct","synthetic":true,"types":[]},{"text":"impl Send for HirObject","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for HirContext&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for SpannedIdentifier","synthetic":true,"types":[]},{"text":"impl Send for IdentifierPath","synthetic":true,"types":[]},{"text":"impl Send for DebrisParser","synthetic":true,"types":[]},{"text":"impl Send for Rule","synthetic":true,"types":[]},{"text":"impl Send for Llir","synthetic":true,"types":[]},{"text":"impl Send for Function","synthetic":true,"types":[]},{"text":"impl Send for FastStore","synthetic":true,"types":[]},{"text":"impl Send for FastStoreFromResult","synthetic":true,"types":[]},{"text":"impl Send for BinaryOperation","synthetic":true,"types":[]},{"text":"impl Send for Call","synthetic":true,"types":[]},{"text":"impl Send for Condition","synthetic":true,"types":[]},{"text":"impl Send for Branch","synthetic":true,"types":[]},{"text":"impl Send for Execute","synthetic":true,"types":[]},{"text":"impl Send for Node","synthetic":true,"types":[]},{"text":"impl Send for Scoreboard","synthetic":true,"types":[]},{"text":"impl Send for ScoreboardValue","synthetic":true,"types":[]},{"text":"impl Send for ScoreboardOperation","synthetic":true,"types":[]},{"text":"impl Send for ScoreboardComparison","synthetic":true,"types":[]},{"text":"impl Send for ItemId","synthetic":true,"types":[]},{"text":"impl Send for ItemIdentifier","synthetic":true,"types":[]},{"text":"impl !Send for MirValue","synthetic":true,"types":[]},{"text":"impl !Send for MirCall","synthetic":true,"types":[]},{"text":"impl Send for MirGotoContext","synthetic":true,"types":[]},{"text":"impl !Send for MirNode","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; !Send for MirInfo&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; !Send for MirContextInfo&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl !Send for MirNamespaceEntry","synthetic":true,"types":[]},{"text":"impl&lt;'ctx&gt; !Send for MirContext&lt;'ctx&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'ctx&gt; !Send for MirBuilder&lt;'a, 'ctx&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'ctx&gt; !Send for Mir&lt;'ctx&gt;","synthetic":true,"types":[]},{"text":"impl !Send for CompileContext","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Send for Namespace&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Send,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl !Send for ObjectRef","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; !Send for DebrisObject&lt;T&gt;","synthetic":true,"types":[]},{"text":"impl Send for ObjBool","synthetic":true,"types":[]},{"text":"impl Send for ObjStaticBool","synthetic":true,"types":[]},{"text":"impl !Send for ObjClass","synthetic":true,"types":[]},{"text":"impl !Send for ObjFunction","synthetic":true,"types":[]},{"text":"impl !Send for FunctionParameters","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Send for FunctionContext&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl !Send for FunctionSignature","synthetic":true,"types":[]},{"text":"impl Send for ObjStaticInt","synthetic":true,"types":[]},{"text":"impl Send for ObjInt","synthetic":true,"types":[]},{"text":"impl !Send for ObjModule","synthetic":true,"types":[]},{"text":"impl !Send for ModuleFactory","synthetic":true,"types":[]},{"text":"impl Send for ObjNull","synthetic":true,"types":[]},{"text":"impl Send for ObjString","synthetic":true,"types":[]},{"text":"impl Send for BuildMode","synthetic":true,"types":[]},{"text":"impl Send for Config","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for SnippetOwned&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Send for SliceOwned&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for SourceAnnotationOwned","synthetic":true,"types":[]},{"text":"impl Send for ParseError","synthetic":true,"types":[]},{"text":"impl !Send for LangError","synthetic":true,"types":[]},{"text":"impl !Send for LangErrorKind","synthetic":true,"types":[]},{"text":"impl !Send for CompileError","synthetic":true,"types":[]},{"text":"impl Send for Type","synthetic":true,"types":[]},{"text":"impl !Send for DebrisFunctionInterface","synthetic":true,"types":[]}];
implementors["debris_lang"] = [{"text":"impl !Send for CompileConfig","synthetic":true,"types":[]}];
implementors["vfs"] = [{"text":"impl&lt;'a&gt; Send for FsElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Send for File","synthetic":true,"types":[]},{"text":"impl Send for Directory","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()