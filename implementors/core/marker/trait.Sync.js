(function() {var implementors = {};
implementors["debris_backends"] = [{"text":"impl !Sync for MinecraftCommand","synthetic":true,"types":[]},{"text":"impl !Sync for ExecuteComponent","synthetic":true,"types":[]},{"text":"impl Sync for MinecraftRange","synthetic":true,"types":[]},{"text":"impl !Sync for ScoreboardPlayer","synthetic":true,"types":[]},{"text":"impl Sync for ObjectiveCriterion","synthetic":true,"types":[]},{"text":"impl !Sync for FunctionIdent","synthetic":true,"types":[]},{"text":"impl !Sync for DatapackBackend","synthetic":true,"types":[]}];
implementors["debris_common"] = [{"text":"impl Sync for Ident","synthetic":true,"types":[]},{"text":"impl Sync for SpecialIdent","synthetic":true,"types":[]},{"text":"impl Sync for Accessor","synthetic":true,"types":[]},{"text":"impl Sync for Code","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Sync for CodeRef&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Sync for InputFiles","synthetic":true,"types":[]},{"text":"impl Sync for Span","synthetic":true,"types":[]}];
implementors["debris_core"] = [{"text":"impl&lt;'code&gt; Sync for Hir&lt;'code&gt;","synthetic":true,"types":[]},{"text":"impl Sync for HirConstValue","synthetic":true,"types":[]},{"text":"impl Sync for HirComparisonOperator","synthetic":true,"types":[]},{"text":"impl Sync for HirInfixOperator","synthetic":true,"types":[]},{"text":"impl Sync for HirInfix","synthetic":true,"types":[]},{"text":"impl Sync for HirPrefixOperator","synthetic":true,"types":[]},{"text":"impl Sync for HirPrefix","synthetic":true,"types":[]},{"text":"impl Sync for HirVariableDeclaration","synthetic":true,"types":[]},{"text":"impl Sync for HirPropertyDeclaration","synthetic":true,"types":[]},{"text":"impl Sync for HirFunctionCall","synthetic":true,"types":[]},{"text":"impl Sync for HirExpression","synthetic":true,"types":[]},{"text":"impl Sync for HirStatement","synthetic":true,"types":[]},{"text":"impl Sync for HirFunction","synthetic":true,"types":[]},{"text":"impl Sync for HirBlock","synthetic":true,"types":[]},{"text":"impl Sync for HirStruct","synthetic":true,"types":[]},{"text":"impl Sync for HirObject","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Sync for HirContext&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Sync for SpannedIdentifier","synthetic":true,"types":[]},{"text":"impl Sync for IdentifierPath","synthetic":true,"types":[]},{"text":"impl Sync for DebrisParser","synthetic":true,"types":[]},{"text":"impl Sync for Rule","synthetic":true,"types":[]},{"text":"impl !Sync for Llir","synthetic":true,"types":[]},{"text":"impl Sync for Function","synthetic":true,"types":[]},{"text":"impl Sync for FastStore","synthetic":true,"types":[]},{"text":"impl Sync for FastStoreFromResult","synthetic":true,"types":[]},{"text":"impl Sync for BinaryOperation","synthetic":true,"types":[]},{"text":"impl Sync for Call","synthetic":true,"types":[]},{"text":"impl Sync for Condition","synthetic":true,"types":[]},{"text":"impl Sync for Branch","synthetic":true,"types":[]},{"text":"impl Sync for Execute","synthetic":true,"types":[]},{"text":"impl Sync for Node","synthetic":true,"types":[]},{"text":"impl Sync for Scoreboard","synthetic":true,"types":[]},{"text":"impl Sync for ScoreboardValue","synthetic":true,"types":[]},{"text":"impl Sync for ScoreboardOperation","synthetic":true,"types":[]},{"text":"impl Sync for ScoreboardComparison","synthetic":true,"types":[]},{"text":"impl Sync for ItemId","synthetic":true,"types":[]},{"text":"impl Sync for ItemIdentifier","synthetic":true,"types":[]},{"text":"impl !Sync for MirValue","synthetic":true,"types":[]},{"text":"impl !Sync for MirNode","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; !Sync for MirInfo&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; !Sync for MirContextInfo&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl !Sync for MirNamespaceEntry","synthetic":true,"types":[]},{"text":"impl&lt;'code&gt; !Sync for MirContext&lt;'code&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; !Sync for MirBuilder&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'code&gt; !Sync for Mir&lt;'code&gt;","synthetic":true,"types":[]},{"text":"impl !Sync for CompileContext","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Sync for Namespace&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Sync,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl !Sync for ObjectRef","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; !Sync for DebrisObject&lt;T&gt;","synthetic":true,"types":[]},{"text":"impl Sync for ObjBool","synthetic":true,"types":[]},{"text":"impl Sync for ObjStaticBool","synthetic":true,"types":[]},{"text":"impl !Sync for ObjClass","synthetic":true,"types":[]},{"text":"impl !Sync for ObjFunction","synthetic":true,"types":[]},{"text":"impl Sync for CallbackFunction","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; !Sync for FunctionContext&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl !Sync for FunctionSignature","synthetic":true,"types":[]},{"text":"impl Sync for ObjStaticInt","synthetic":true,"types":[]},{"text":"impl Sync for ObjInt","synthetic":true,"types":[]},{"text":"impl !Sync for ObjModule","synthetic":true,"types":[]},{"text":"impl !Sync for ModuleFactory","synthetic":true,"types":[]},{"text":"impl Sync for ObjNull","synthetic":true,"types":[]},{"text":"impl Sync for ObjString","synthetic":true,"types":[]},{"text":"impl Sync for BuildMode","synthetic":true,"types":[]},{"text":"impl Sync for Config","synthetic":true,"types":[]},{"text":"impl !Sync for CompileError","synthetic":true,"types":[]},{"text":"impl Sync for ParseError","synthetic":true,"types":[]},{"text":"impl !Sync for LangError","synthetic":true,"types":[]},{"text":"impl !Sync for LangErrorKind","synthetic":true,"types":[]},{"text":"impl Sync for Type","synthetic":true,"types":[]}];
implementors["debris_lang"] = [{"text":"impl !Sync for CompileConfig","synthetic":true,"types":[]}];
implementors["vfs"] = [{"text":"impl&lt;'a&gt; Sync for FsElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Sync for File","synthetic":true,"types":[]},{"text":"impl Sync for Directory","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()