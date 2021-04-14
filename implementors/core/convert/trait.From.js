(function() {var implementors = {};
implementors["debris_common"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_common/enum.SpecialIdent.html\" title=\"enum debris_common::SpecialIdent\">SpecialIdent</a>&gt; for <a class=\"enum\" href=\"debris_common/enum.Ident.html\" title=\"enum debris_common::Ident\">Ident</a>","synthetic":false,"types":["debris_common::ident::Ident"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;T&gt; for <a class=\"enum\" href=\"debris_common/enum.Ident.html\" title=\"enum debris_common::Ident\">Ident</a> <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.Into.html\" title=\"trait core::convert::Into\">Into</a>&lt;SmolStr&gt;,&nbsp;</span>","synthetic":false,"types":["debris_common::ident::Ident"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_common/enum.Ident.html\" title=\"enum debris_common::Ident\">Ident</a>&gt; for <a class=\"enum\" href=\"debris_common/enum.Accessor.html\" title=\"enum debris_common::Accessor\">Accessor</a>","synthetic":false,"types":["debris_common::accessor::Accessor"]}];
implementors["debris_core"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_common/span/struct.Span.html\" title=\"struct debris_common::span::Span\">Span</a>&gt; for <a class=\"struct\" href=\"debris_core/hir/struct.SpannedIdentifier.html\" title=\"struct debris_core::hir::SpannedIdentifier\">SpannedIdentifier</a>","synthetic":false,"types":["debris_core::hir::identifier::SpannedIdentifier"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/hir/struct.SpannedIdentifier.html\" title=\"struct debris_core::hir::SpannedIdentifier\">SpannedIdentifier</a>&gt; for <a class=\"struct\" href=\"debris_core/hir/struct.IdentifierPath.html\" title=\"struct debris_core::hir::IdentifierPath\">IdentifierPath</a>","synthetic":false,"types":["debris_core::hir::identifier::IdentifierPath"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_core/llir/json_format/enum.JsonFormatComponent.html\" title=\"enum debris_core::llir::json_format::JsonFormatComponent\">JsonFormatComponent</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_core/llir/json_format/struct.FormattedText.html\" title=\"struct debris_core::llir::json_format::FormattedText\">FormattedText</a>","synthetic":false,"types":["debris_core::llir::json_format::FormattedText"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/struct.ObjectRef.html\" title=\"struct debris_core::ObjectRef\">ObjectRef</a>&gt; for <a class=\"enum\" href=\"debris_core/mir/enum.MirValue.html\" title=\"enum debris_core::mir::MirValue\">MirValue</a>","synthetic":false,"types":["debris_core::mir::mir_nodes::MirValue"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;Index&gt; for <a class=\"struct\" href=\"debris_core/mir/struct.ContextId.html\" title=\"struct debris_core::mir::ContextId\">ContextId</a>","synthetic":false,"types":["debris_core::mir::mir_context::ContextId"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_core/hir/hir_nodes/enum.HirControlKind.html\" title=\"enum debris_core::hir::hir_nodes::HirControlKind\">HirControlKind</a>&gt; for <a class=\"enum\" href=\"debris_core/mir/enum.ControlFlowMode.html\" title=\"enum debris_core::mir::ControlFlowMode\">ControlFlowMode</a>","synthetic":false,"types":["debris_core::mir::mir_control_flow::ControlFlowMode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/mir/struct.ContextId.html\" title=\"struct debris_core::mir::ContextId\">ContextId</a>&gt; for <a class=\"struct\" href=\"debris_core/struct.Namespace.html\" title=\"struct debris_core::Namespace\">Namespace</a>","synthetic":false,"types":["debris_core::namespace::Namespace"]},{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"debris_core/trait.ObjectPayload.html\" title=\"trait debris_core::ObjectPayload\">ObjectPayload</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/struct.DebrisObject.html\" title=\"struct debris_core::DebrisObject\">DebrisObject</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"debris_core/struct.ObjectRef.html\" title=\"struct debris_core::ObjectRef\">ObjectRef</a>","synthetic":false,"types":["debris_core::debris_object::ObjectRef"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/llir/utils/struct.ItemId.html\" title=\"struct debris_core::llir::utils::ItemId\">ItemId</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_bool/struct.ObjBool.html\" title=\"struct debris_core::objects::obj_bool::ObjBool\">ObjBool</a>","synthetic":false,"types":["debris_core::objects::obj_bool::ObjBool"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.bool.html\">bool</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_bool_static/struct.ObjStaticBool.html\" title=\"struct debris_core::objects::obj_bool_static::ObjStaticBool\">ObjStaticBool</a>","synthetic":false,"types":["debris_core::objects::obj_bool_static::ObjStaticBool"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;Class&gt;&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_class/struct.ObjClass.html\" title=\"struct debris_core::objects::obj_class::ObjClass\">ObjClass</a>","synthetic":false,"types":["debris_core::objects::obj_class::ObjClass"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_core/enum.TypePattern.html\" title=\"enum debris_core::TypePattern\">TypePattern</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"enum\" href=\"debris_core/objects/obj_function/enum.FunctionParameters.html\" title=\"enum debris_core::objects::obj_function::FunctionParameters\">FunctionParameters</a>","synthetic":false,"types":["debris_core::objects::obj_function::FunctionParameters"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/llir/utils/struct.ItemId.html\" title=\"struct debris_core::llir::utils::ItemId\">ItemId</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int/struct.ObjInt.html\" title=\"struct debris_core::objects::obj_int::ObjInt\">ObjInt</a>","synthetic":false,"types":["debris_core::objects::obj_int::ObjInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i8.html\">i8</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i16.html\">i16</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i32.html\">i32</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i64.html\">i64</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i128.html\">i128</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.isize.html\">isize</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u16.html\">u16</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u32.html\">u32</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u64.html\">u64</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u128.html\">u128</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.usize.html\">usize</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_core::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_core::objects::obj_int_static::ObjStaticInt"]},{"text":"impl&lt;F:&nbsp;<a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/ops/function/trait.Fn.html\" title=\"trait core::ops::function::Fn\">Fn</a>(&amp;<a class=\"struct\" href=\"debris_core/struct.CompileContext.html\" title=\"struct debris_core::CompileContext\">CompileContext</a>) -&gt; <a class=\"struct\" href=\"debris_core/objects/obj_module/struct.ObjModule.html\" title=\"struct debris_core::objects::obj_module::ObjModule\">ObjModule</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.reference.html\">&amp;'static </a>F&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_module/struct.ModuleFactory.html\" title=\"struct debris_core::objects::obj_module::ModuleFactory\">ModuleFactory</a>","synthetic":false,"types":["debris_core::objects::obj_module::ModuleFactory"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.unit.html\">()</a>&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_null/struct.ObjNull.html\" title=\"struct debris_core::objects::obj_null::ObjNull\">ObjNull</a>","synthetic":false,"types":["debris_core::objects::obj_null::ObjNull"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;&gt; for <a class=\"struct\" href=\"debris_core/objects/obj_string/struct.ObjString.html\" title=\"struct debris_core::objects::obj_string::ObjString\">ObjString</a>","synthetic":false,"types":["debris_core::objects::obj_string::ObjString"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/error/struct.ParseError.html\" title=\"struct debris_core::error::ParseError\">ParseError</a>&gt; for <a class=\"enum\" href=\"debris_core/error/enum.CompileError.html\" title=\"enum debris_core::error::CompileError\">CompileError</a>","synthetic":false,"types":["debris_core::error::CompileError"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_core/error/struct.LangError.html\" title=\"struct debris_core::error::LangError\">LangError</a>&gt; for <a class=\"enum\" href=\"debris_core/error/enum.CompileError.html\" title=\"enum debris_core::error::CompileError\">CompileError</a>","synthetic":false,"types":["debris_core::error::CompileError"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;Class&gt;&gt; for <a class=\"enum\" href=\"debris_core/enum.TypePattern.html\" title=\"enum debris_core::TypePattern\">TypePattern</a>","synthetic":false,"types":["debris_core::types::TypePattern"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/boxed/struct.Box.html\" title=\"struct alloc::boxed::Box\">Box</a>&lt;dyn <a class=\"trait\" href=\"debris_core/function_interface/trait.NormalizedFunctionInterface.html\" title=\"trait debris_core::function_interface::NormalizedFunctionInterface\">NormalizedFunctionInterface</a> + 'static, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_core/function_interface/struct.DebrisFunctionInterface.html\" title=\"struct debris_core::function_interface::DebrisFunctionInterface\">DebrisFunctionInterface</a>","synthetic":false,"types":["debris_core::function_interface::DebrisFunctionInterface"]},{"text":"impl&lt;F&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;F&gt; for <a class=\"struct\" href=\"debris_core/function_interface/struct.DebrisFunctionInterface.html\" title=\"struct debris_core::function_interface::DebrisFunctionInterface\">DebrisFunctionInterface</a> <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;F: <a class=\"trait\" href=\"debris_core/function_interface/trait.NormalizedFunctionInterface.html\" title=\"trait debris_core::function_interface::NormalizedFunctionInterface\">NormalizedFunctionInterface</a> + 'static,&nbsp;</span>","synthetic":false,"types":["debris_core::function_interface::DebrisFunctionInterface"]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()