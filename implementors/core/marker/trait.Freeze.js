(function() {var implementors = {};
implementors["debris_backends"] = [{"text":"impl Freeze for MinecraftCommand","synthetic":true,"types":[]},{"text":"impl Freeze for ExecuteComponent","synthetic":true,"types":[]},{"text":"impl Freeze for MinecraftRange","synthetic":true,"types":[]},{"text":"impl Freeze for ScoreboardPlayer","synthetic":true,"types":[]},{"text":"impl Freeze for ObjectiveCriterion","synthetic":true,"types":[]},{"text":"impl Freeze for FunctionIdent","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for DatapackBackend&lt;'a&gt;","synthetic":true,"types":[]}];
implementors["debris_common"] = [{"text":"impl Freeze for Ident","synthetic":true,"types":[]},{"text":"impl Freeze for SpecialIdent","synthetic":true,"types":[]},{"text":"impl Freeze for Accessor","synthetic":true,"types":[]},{"text":"impl Freeze for Code","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for CodeRef&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for InputFiles","synthetic":true,"types":[]},{"text":"impl Freeze for Span","synthetic":true,"types":[]}];
implementors["debris_core"] = [{"text":"impl&lt;'code&gt; Freeze for Hir&lt;'code&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for HirConstValue","synthetic":true,"types":[]},{"text":"impl Freeze for HirComparisonOperator","synthetic":true,"types":[]},{"text":"impl Freeze for HirInfixOperator","synthetic":true,"types":[]},{"text":"impl Freeze for HirInfix","synthetic":true,"types":[]},{"text":"impl Freeze for HirPrefixOperator","synthetic":true,"types":[]},{"text":"impl Freeze for HirPrefix","synthetic":true,"types":[]},{"text":"impl Freeze for HirVariableDeclaration","synthetic":true,"types":[]},{"text":"impl Freeze for HirVariableInitialization","synthetic":true,"types":[]},{"text":"impl Freeze for HirPropertyDeclaration","synthetic":true,"types":[]},{"text":"impl Freeze for HirFunctionCall","synthetic":true,"types":[]},{"text":"impl Freeze for HirExpression","synthetic":true,"types":[]},{"text":"impl Freeze for HirStatement","synthetic":true,"types":[]},{"text":"impl Freeze for HirTypePattern","synthetic":true,"types":[]},{"text":"impl Freeze for HirFunction","synthetic":true,"types":[]},{"text":"impl Freeze for HirBlock","synthetic":true,"types":[]},{"text":"impl Freeze for HirStruct","synthetic":true,"types":[]},{"text":"impl Freeze for HirObject","synthetic":true,"types":[]},{"text":"impl Freeze for HirItem","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for HirContext&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for SpannedIdentifier","synthetic":true,"types":[]},{"text":"impl Freeze for IdentifierPath","synthetic":true,"types":[]},{"text":"impl Freeze for DebrisParser","synthetic":true,"types":[]},{"text":"impl Freeze for Rule","synthetic":true,"types":[]},{"text":"impl Freeze for Llir","synthetic":true,"types":[]},{"text":"impl Freeze for Function","synthetic":true,"types":[]},{"text":"impl Freeze for FastStore","synthetic":true,"types":[]},{"text":"impl Freeze for FastStoreFromResult","synthetic":true,"types":[]},{"text":"impl Freeze for BinaryOperation","synthetic":true,"types":[]},{"text":"impl Freeze for Call","synthetic":true,"types":[]},{"text":"impl Freeze for Condition","synthetic":true,"types":[]},{"text":"impl Freeze for Branch","synthetic":true,"types":[]},{"text":"impl Freeze for Execute","synthetic":true,"types":[]},{"text":"impl Freeze for Node","synthetic":true,"types":[]},{"text":"impl Freeze for Scoreboard","synthetic":true,"types":[]},{"text":"impl Freeze for ScoreboardValue","synthetic":true,"types":[]},{"text":"impl Freeze for ScoreboardOperation","synthetic":true,"types":[]},{"text":"impl Freeze for ScoreboardComparison","synthetic":true,"types":[]},{"text":"impl Freeze for ItemId","synthetic":true,"types":[]},{"text":"impl Freeze for ItemIdentifier","synthetic":true,"types":[]},{"text":"impl Freeze for MirValue","synthetic":true,"types":[]},{"text":"impl Freeze for MirCall","synthetic":true,"types":[]},{"text":"impl Freeze for MirGotoContext","synthetic":true,"types":[]},{"text":"impl Freeze for MirNode","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; Freeze for MirInfo&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'code&gt; Freeze for MirContextInfo&lt;'a, 'code&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for MirNamespaceEntry","synthetic":true,"types":[]},{"text":"impl Freeze for NamespaceArena","synthetic":true,"types":[]},{"text":"impl&lt;'ctx&gt; Freeze for MirContext&lt;'ctx&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a, 'ctx&gt; Freeze for MirBuilder&lt;'a, 'ctx&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'ctx&gt; Freeze for Mir&lt;'ctx&gt;","synthetic":true,"types":[]},{"text":"impl !Freeze for CompileContext","synthetic":true,"types":[]},{"text":"impl&lt;T&gt; Freeze for Namespace&lt;T&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for ObjectRef","synthetic":true,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized&gt; Freeze for DebrisObject&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Freeze,&nbsp;</span>","synthetic":true,"types":[]},{"text":"impl Freeze for ObjBool","synthetic":true,"types":[]},{"text":"impl Freeze for ObjStaticBool","synthetic":true,"types":[]},{"text":"impl !Freeze for ObjClass","synthetic":true,"types":[]},{"text":"impl Freeze for ObjFunction","synthetic":true,"types":[]},{"text":"impl Freeze for FunctionParameters","synthetic":true,"types":[]},{"text":"impl&lt;'llir, 'ctx, 'ns&gt; Freeze for FunctionContext&lt;'llir, 'ctx, 'ns&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for FunctionSignature","synthetic":true,"types":[]},{"text":"impl Freeze for FunctionParameterDefinition","synthetic":true,"types":[]},{"text":"impl Freeze for ObjNativeFunction","synthetic":true,"types":[]},{"text":"impl Freeze for ObjNativeFunctionSignature","synthetic":true,"types":[]},{"text":"impl Freeze for ObjStaticInt","synthetic":true,"types":[]},{"text":"impl Freeze for ObjInt","synthetic":true,"types":[]},{"text":"impl Freeze for ObjModule","synthetic":true,"types":[]},{"text":"impl Freeze for ModuleFactory","synthetic":true,"types":[]},{"text":"impl Freeze for ObjNull","synthetic":true,"types":[]},{"text":"impl Freeze for ObjString","synthetic":true,"types":[]},{"text":"impl Freeze for BuildMode","synthetic":true,"types":[]},{"text":"impl Freeze for Config","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for SnippetOwned&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl&lt;'a&gt; Freeze for SliceOwned&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for SourceAnnotationOwned","synthetic":true,"types":[]},{"text":"impl Freeze for ParseError","synthetic":true,"types":[]},{"text":"impl Freeze for LangError","synthetic":true,"types":[]},{"text":"impl Freeze for LangErrorKind","synthetic":true,"types":[]},{"text":"impl Freeze for CompileError","synthetic":true,"types":[]},{"text":"impl Freeze for TypePattern","synthetic":true,"types":[]},{"text":"impl Freeze for Type","synthetic":true,"types":[]},{"text":"impl Freeze for DebrisFunctionInterface","synthetic":true,"types":[]}];
implementors["debris_lang"] = [{"text":"impl !Freeze for CompileConfig","synthetic":true,"types":[]}];
implementors["vfs"] = [{"text":"impl&lt;'a&gt; Freeze for FsElement&lt;'a&gt;","synthetic":true,"types":[]},{"text":"impl Freeze for File","synthetic":true,"types":[]},{"text":"impl Freeze for Directory","synthetic":true,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()