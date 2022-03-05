(function() {var implementors = {};
implementors["debris_backends"] = [{"text":"impl&lt;'a&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;&amp;'a <a class=\"struct\" href=\"debris_common/config/struct.Config.html\" title=\"struct debris_common::config::Config\">Config</a>&gt; for <a class=\"struct\" href=\"debris_backends/datapack/templates/struct.TemplateData.html\" title=\"struct debris_backends::datapack::templates::TemplateData\">TemplateData</a>&lt;'a&gt;","synthetic":false,"types":["debris_backends::datapack::templates::TemplateData"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>&gt; for <a class=\"enum\" href=\"debris_backends/datapack/scoreboard_context/enum.ScoreboardPlayerId.html\" title=\"enum debris_backends::datapack::scoreboard_context::ScoreboardPlayerId\">ScoreboardPlayerId</a>","synthetic":false,"types":["debris_backends::datapack::scoreboard_context::ScoreboardPlayerId"]}];
implementors["debris_common"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_common/ident/enum.SpecialIdent.html\" title=\"enum debris_common::ident::SpecialIdent\">SpecialIdent</a>&gt; for <a class=\"enum\" href=\"debris_common/ident/enum.Ident.html\" title=\"enum debris_common::ident::Ident\">Ident</a>","synthetic":false,"types":["debris_common::ident::Ident"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;T&gt; for <a class=\"enum\" href=\"debris_common/ident/enum.Ident.html\" title=\"enum debris_common::ident::Ident\">Ident</a> <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.Into.html\" title=\"trait core::convert::Into\">Into</a>&lt;SmolStr&gt;,&nbsp;</span>","synthetic":false,"types":["debris_common::ident::Ident"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_common/ident/enum.Ident.html\" title=\"enum debris_common::ident::Ident\">Ident</a>&gt; for <a class=\"enum\" href=\"debris_common/accessor/enum.Accessor.html\" title=\"enum debris_common::accessor::Accessor\">Accessor</a>","synthetic":false,"types":["debris_common::accessor::Accessor"]}];
implementors["debris_error"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_error/parse_error/struct.ParseError.html\" title=\"struct debris_error::parse_error::ParseError\">ParseError</a>&gt; for <a class=\"enum\" href=\"debris_error/enum.CompileError.html\" title=\"enum debris_error::CompileError\">CompileError</a>","synthetic":false,"types":["debris_error::CompileError"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_error/lang_error/struct.LangError.html\" title=\"struct debris_error::lang_error::LangError\">LangError</a>&gt; for <a class=\"enum\" href=\"debris_error/enum.CompileError.html\" title=\"enum debris_error::CompileError\">CompileError</a>","synthetic":false,"types":["debris_error::CompileError"]}];
implementors["debris_hir"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_common/span/struct.Span.html\" title=\"struct debris_common::span::Span\">Span</a>&gt; for <a class=\"struct\" href=\"debris_hir/identifier/struct.SpannedIdentifier.html\" title=\"struct debris_hir::identifier::SpannedIdentifier\">SpannedIdentifier</a>","synthetic":false,"types":["debris_hir::identifier::SpannedIdentifier"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_hir/identifier/struct.SpannedIdentifier.html\" title=\"struct debris_hir::identifier::SpannedIdentifier\">SpannedIdentifier</a>&gt; for <a class=\"struct\" href=\"debris_hir/identifier/struct.IdentifierPath.html\" title=\"struct debris_hir::identifier::IdentifierPath\">IdentifierPath</a>","synthetic":false,"types":["debris_hir::identifier::IdentifierPath"]}];
implementors["debris_llir"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_llir/types/enum.Type.html\" title=\"enum debris_llir::types::Type\">Type</a>&gt; for <a class=\"enum\" href=\"debris_llir/class/enum.ClassKind.html\" title=\"enum debris_llir::class::ClassKind\">ClassKind</a>","synthetic":false,"types":["debris_llir::class::ClassKind"]},{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"debris_llir/debris_object/trait.ObjectPayload.html\" title=\"trait debris_llir::debris_object::ObjectPayload\">ObjectPayload</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/debris_object/struct.DebrisObject.html\" title=\"struct debris_llir::debris_object::DebrisObject\">DebrisObject</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"debris_llir/debris_object/struct.ObjectRef.html\" title=\"struct debris_llir::debris_object::ObjectRef\">ObjectRef</a>","synthetic":false,"types":["debris_llir::debris_object::ObjectRef"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/function_interface/struct.NormalizedFunction.html\" title=\"struct debris_llir::function_interface::NormalizedFunction\">NormalizedFunction</a>&gt; for <a class=\"struct\" href=\"debris_llir/function_interface/struct.DebrisFunctionInterface.html\" title=\"struct debris_llir::function_interface::DebrisFunctionInterface\">DebrisFunctionInterface</a>","synthetic":false,"types":["debris_llir::function_interface::DebrisFunctionInterface"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_llir/json_format/enum.JsonFormatComponent.html\" title=\"enum debris_llir::json_format::JsonFormatComponent\">JsonFormatComponent</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/json_format/struct.FormattedText.html\" title=\"struct debris_llir::json_format::FormattedText\">FormattedText</a>","synthetic":false,"types":["debris_llir::json_format::FormattedText"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"enum\" href=\"debris_llir/memory/enum.MemoryLayout.html\" title=\"enum debris_llir::memory::MemoryLayout\">MemoryLayout</a>","synthetic":false,"types":["debris_llir::memory::MemoryLayout"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_bool/struct.ObjBool.html\" title=\"struct debris_llir::objects::obj_bool::ObjBool\">ObjBool</a>","synthetic":false,"types":["debris_llir::objects::obj_bool::ObjBool"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.bool.html\">bool</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_bool_static/struct.ObjStaticBool.html\" title=\"struct debris_llir::objects::obj_bool_static::ObjStaticBool\">ObjStaticBool</a>","synthetic":false,"types":["debris_llir::objects::obj_bool_static::ObjStaticBool"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"struct\" href=\"debris_llir/class/struct.Class.html\" title=\"struct debris_llir::class::Class\">Class</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_class/struct.ObjClass.html\" title=\"struct debris_llir::objects::obj_class::ObjClass\">ObjClass</a>","synthetic":false,"types":["debris_llir::objects::obj_class::ObjClass"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_format_string/struct.ObjFormatString.html\" title=\"struct debris_llir::objects::obj_format_string::ObjFormatString\">ObjFormatString</a>","synthetic":false,"types":["debris_llir::objects::obj_format_string::ObjFormatString"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int/struct.ObjInt.html\" title=\"struct debris_llir::objects::obj_int::ObjInt\">ObjInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int::ObjInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i8.html\">i8</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i16.html\">i16</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i32.html\">i32</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i64.html\">i64</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i128.html\">i128</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.isize.html\">isize</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u16.html\">u16</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u32.html\">u32</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u64.html\">u64</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u128.html\">u128</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.usize.html\">usize</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>","synthetic":false,"types":["debris_llir::objects::obj_int_static::ObjStaticInt"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.unit.html\">()</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_null/struct.ObjNull.html\" title=\"struct debris_llir::objects::obj_null::ObjNull\">ObjNull</a>","synthetic":false,"types":["debris_llir::objects::obj_null::ObjNull"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_string/struct.ObjString.html\" title=\"struct debris_llir::objects::obj_string::ObjString\">ObjString</a>","synthetic":false,"types":["debris_llir::objects::obj_string::ObjString"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_llir/types/enum.TypePattern.html\" title=\"enum debris_llir::types::TypePattern\">TypePattern</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_tuple_object/struct.Tuple.html\" title=\"struct debris_llir::objects::obj_tuple_object::Tuple\">Tuple</a>","synthetic":false,"types":["debris_llir::objects::obj_tuple_object::Tuple"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;&amp;'_ <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/collections/hash/map/struct.HashMap.html\" title=\"struct std::collections::hash::map::HashMap\">HashMap</a>&lt;<a class=\"struct\" href=\"debris_llir/block_id/struct.BlockId.html\" title=\"struct debris_llir::block_id::BlockId\">BlockId</a>, <a class=\"struct\" href=\"debris_llir/llir_nodes/struct.Function.html\" title=\"struct debris_llir::llir_nodes::Function\">Function</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/core/hash/struct.BuildHasherDefault.html\" title=\"struct core::hash::BuildHasherDefault\">BuildHasherDefault</a>&lt;FxHasher&gt;&gt;&gt; for <a class=\"struct\" href=\"debris_llir/opt/call_graph/struct.CallGraph.html\" title=\"struct debris_llir::opt::call_graph::CallGraph\">CallGraph</a>","synthetic":false,"types":["debris_llir::opt::call_graph::CallGraph"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i32.html\">i32</a>&gt; for <a class=\"struct\" href=\"debris_llir/opt/optimizers/arithmetic_optimizer/struct.Fraction.html\" title=\"struct debris_llir::opt::optimizers::arithmetic_optimizer::Fraction\">Fraction</a>","synthetic":false,"types":["debris_llir::opt::optimizers::arithmetic_optimizer::Fraction"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.bool.html\">bool</a>&gt; for <a class=\"enum\" href=\"debris_llir/opt/optimizers/redundancy_optimizer/enum.SimplifiedCondition.html\" title=\"enum debris_llir::opt::optimizers::redundancy_optimizer::SimplifiedCondition\">SimplifiedCondition</a>","synthetic":false,"types":["debris_llir::opt::optimizers::redundancy_optimizer::SimplifiedCondition"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"struct\" href=\"debris_llir/class/struct.Class.html\" title=\"struct debris_llir::class::Class\">Class</a>&gt;&gt; for <a class=\"enum\" href=\"debris_llir/types/enum.TypePattern.html\" title=\"enum debris_llir::types::TypePattern\">TypePattern</a>","synthetic":false,"types":["debris_llir::types::TypePattern"]}];
implementors["debris_mir"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_mir/mir_context/enum.ReturnContext.html\" title=\"enum debris_mir::mir_context::ReturnContext\">ReturnContext</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_builder/enum.ReturnContextBehavior.html\" title=\"enum debris_mir::mir_builder::ReturnContextBehavior\">ReturnContextBehavior</a>","synthetic":false,"types":["debris_mir::mir_builder::ReturnContextBehavior"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.Branch.html\" title=\"struct debris_mir::mir_nodes::Branch\">Branch</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.FunctionCall.html\" title=\"struct debris_mir::mir_nodes::FunctionCall\">FunctionCall</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.Goto.html\" title=\"struct debris_mir::mir_nodes::Goto\">Goto</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.RuntimePromotion.html\" title=\"struct debris_mir::mir_nodes::RuntimePromotion\">RuntimePromotion</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.RuntimeCopy.html\" title=\"struct debris_mir::mir_nodes::RuntimeCopy\">RuntimeCopy</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VerifyValueComptime.html\" title=\"struct debris_mir::mir_nodes::VerifyValueComptime\">VerifyValueComptime</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VerifyTupleLength.html\" title=\"struct debris_mir::mir_nodes::VerifyTupleLength\">VerifyTupleLength</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VerifyPropertyExists.html\" title=\"struct debris_mir::mir_nodes::VerifyPropertyExists\">VerifyPropertyExists</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.PrimitiveDeclaration.html\" title=\"struct debris_mir::mir_nodes::PrimitiveDeclaration\">PrimitiveDeclaration</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VariableUpdate.html\" title=\"struct debris_mir::mir_nodes::VariableUpdate\">VariableUpdate</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.PropertyAccess.html\" title=\"struct debris_mir::mir_nodes::PropertyAccess\">PropertyAccess</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>","synthetic":false,"types":["debris_mir::mir_nodes::MirNode"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_mir/mir_primitives/enum.MirFormatStringComponent.html\" title=\"enum debris_mir::mir_primitives::MirFormatStringComponent\">MirFormatStringComponent</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_mir/mir_primitives/struct.MirFormatString.html\" title=\"struct debris_mir::mir_primitives::MirFormatString\">MirFormatString</a>","synthetic":false,"types":["debris_mir::mir_primitives::MirFormatString"]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()