(function() {var implementors = {};
implementors["debris_common"] = [{"text":"impl From&lt;SpecialIdent&gt; for Ident","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; From&lt;T&gt; for Ident <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Into&lt;SmolStr&gt;,&nbsp;</span>","synthetic":false,"types":[]},{"text":"impl From&lt;Ident&gt; for Accessor","synthetic":false,"types":[]}];
implementors["debris_core"] = [{"text":"impl From&lt;LocalSpan&gt; for SpannedIdentifier","synthetic":false,"types":[]},{"text":"impl&lt;'a&gt; From&lt;Span&lt;'a&gt;&gt; for SpannedIdentifier","synthetic":false,"types":[]},{"text":"impl From&lt;SpannedIdentifier&gt; for IdentifierPath","synthetic":false,"types":[]},{"text":"impl From&lt;ObjectRef&gt; for MirValue","synthetic":false,"types":[]},{"text":"impl&lt;T:&nbsp;ObjectPayload&gt; From&lt;DebrisObject&lt;T&gt;&gt; for ObjectRef","synthetic":false,"types":[]},{"text":"impl From&lt;ItemId&gt; for ObjBool","synthetic":false,"types":[]},{"text":"impl From&lt;bool&gt; for ObjStaticBool","synthetic":false,"types":[]},{"text":"impl From&lt;fn(&amp;mut FunctionContext&lt;'_&gt;, &amp;[ObjectRef]) -&gt; Result&lt;ObjectRef, LangErrorKind&gt;&gt; for CallbackFunction","synthetic":false,"types":[]},{"text":"impl From&lt;i8&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;i16&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;i32&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;i64&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;i128&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;isize&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;u8&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;u16&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;u32&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;u64&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;u128&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;usize&gt; for ObjStaticInt","synthetic":false,"types":[]},{"text":"impl From&lt;ItemId&gt; for ObjInt","synthetic":false,"types":[]},{"text":"impl&lt;F:&nbsp;Fn(&amp;CompileContext) -&gt; ObjModule&gt; From&lt;&amp;'static F&gt; for ModuleFactory","synthetic":false,"types":[]},{"text":"impl From&lt;String&gt; for ObjString","synthetic":false,"types":[]},{"text":"impl From&lt;ParseError&gt; for CompileError","synthetic":false,"types":[]},{"text":"impl From&lt;LangError&gt; for CompileError","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()