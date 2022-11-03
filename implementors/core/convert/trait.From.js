(function() {var implementors = {
"debris_backends":[["impl&lt;'a&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;&amp;'a Config&gt; for <a class=\"struct\" href=\"debris_backends/datapack/templates/struct.TemplateData.html\" title=\"struct debris_backends::datapack::templates::TemplateData\">TemplateData</a>&lt;'a&gt;"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>&gt; for <a class=\"enum\" href=\"debris_backends/datapack/scoreboard_context/enum.ScoreboardPlayerId.html\" title=\"enum debris_backends::datapack::scoreboard_context::ScoreboardPlayerId\">ScoreboardPlayerId</a>"]],
"debris_common":[["impl&lt;T&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;T&gt; for <a class=\"struct\" href=\"debris_common/clone_cell/struct.CloneCell.html\" title=\"struct debris_common::clone_cell::CloneCell\">CloneCell</a>&lt;T&gt;"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_common/ident/enum.SpecialIdent.html\" title=\"enum debris_common::ident::SpecialIdent\">SpecialIdent</a>&gt; for <a class=\"enum\" href=\"debris_common/ident/enum.Ident.html\" title=\"enum debris_common::ident::Ident\">Ident</a>"],["impl&lt;T&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;T&gt; for <a class=\"enum\" href=\"debris_common/ident/enum.Ident.html\" title=\"enum debris_common::ident::Ident\">Ident</a><span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.Into.html\" title=\"trait core::convert::Into\">Into</a>&lt;SmolStr&gt;,</span>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_common/ident/enum.Ident.html\" title=\"enum debris_common::ident::Ident\">Ident</a>&gt; for <a class=\"enum\" href=\"debris_common/accessor/enum.Accessor.html\" title=\"enum debris_common::accessor::Accessor\">Accessor</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/core/ops/range/struct.Range.html\" title=\"struct core::ops::range::Range\">Range</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.usize.html\">usize</a>&gt;&gt; for <a class=\"struct\" href=\"debris_common/span/struct.Span.html\" title=\"struct debris_common::span::Span\">Span</a>"]],
"debris_error":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_error/enum.SingleCompileError.html\" title=\"enum debris_error::SingleCompileError\">SingleCompileError</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_error/struct.CompileErrors.html\" title=\"struct debris_error::CompileErrors\">CompileErrors</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_error/parse_error/enum.ParseError.html\" title=\"enum debris_error::parse_error::ParseError\">ParseError</a>&gt; for <a class=\"enum\" href=\"debris_error/enum.SingleCompileError.html\" title=\"enum debris_error::SingleCompileError\">SingleCompileError</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_error/lang_error/struct.LangError.html\" title=\"struct debris_error::lang_error::LangError\">LangError</a>&gt; for <a class=\"enum\" href=\"debris_error/enum.SingleCompileError.html\" title=\"enum debris_error::SingleCompileError\">SingleCompileError</a>"]],
"debris_hir":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;Span&gt; for <a class=\"struct\" href=\"debris_hir/identifier/struct.SpannedIdentifier.html\" title=\"struct debris_hir::identifier::SpannedIdentifier\">SpannedIdentifier</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_hir/identifier/struct.SpannedIdentifier.html\" title=\"struct debris_hir::identifier::SpannedIdentifier\">SpannedIdentifier</a>&gt; for <a class=\"struct\" href=\"debris_hir/identifier/struct.IdentifierPath.html\" title=\"struct debris_hir::identifier::IdentifierPath\">IdentifierPath</a>"]],
"debris_llir":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_llir/types/enum.Type.html\" title=\"enum debris_llir::types::Type\">Type</a>&gt; for <a class=\"enum\" href=\"debris_llir/class/enum.ClassKind.html\" title=\"enum debris_llir::class::ClassKind\">ClassKind</a>"],["impl&lt;T:&nbsp;<a class=\"trait\" href=\"debris_llir/debris_object/trait.ObjectPayload.html\" title=\"trait debris_llir::debris_object::ObjectPayload\">ObjectPayload</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/debris_object/struct.DebrisObject.html\" title=\"struct debris_llir::debris_object::DebrisObject\">DebrisObject</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"debris_llir/debris_object/struct.ObjectRef.html\" title=\"struct debris_llir::debris_object::ObjectRef\">ObjectRef</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/function_interface/struct.NormalizedFunction.html\" title=\"struct debris_llir::function_interface::NormalizedFunction\">NormalizedFunction</a>&gt; for <a class=\"struct\" href=\"debris_llir/function_interface/struct.DebrisFunctionInterface.html\" title=\"struct debris_llir::function_interface::DebrisFunctionInterface\">DebrisFunctionInterface</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_llir/json_format/enum.JsonFormatComponent.html\" title=\"enum debris_llir::json_format::JsonFormatComponent\">JsonFormatComponent</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/json_format/struct.FormattedText.html\" title=\"struct debris_llir::json_format::FormattedText\">FormattedText</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"enum\" href=\"debris_llir/memory/enum.MemoryLayout.html\" title=\"enum debris_llir::memory::MemoryLayout\">MemoryLayout</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_bool/struct.ObjBool.html\" title=\"struct debris_llir::objects::obj_bool::ObjBool\">ObjBool</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.bool.html\">bool</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_bool_static/struct.ObjStaticBool.html\" title=\"struct debris_llir::objects::obj_bool_static::ObjStaticBool\">ObjStaticBool</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"struct\" href=\"debris_llir/class/struct.Class.html\" title=\"struct debris_llir::class::Class\">Class</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_class/struct.ObjClass.html\" title=\"struct debris_llir::objects::obj_class::ObjClass\">ObjClass</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_format_string/struct.ObjFormatString.html\" title=\"struct debris_llir::objects::obj_format_string::ObjFormatString\">ObjFormatString</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_llir/item_id/struct.ItemId.html\" title=\"struct debris_llir::item_id::ItemId\">ItemId</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int/struct.ObjInt.html\" title=\"struct debris_llir::objects::obj_int::ObjInt\">ObjInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i8.html\">i8</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i16.html\">i16</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i32.html\">i32</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i64.html\">i64</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i128.html\">i128</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.isize.html\">isize</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u8.html\">u8</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u16.html\">u16</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u32.html\">u32</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u64.html\">u64</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.u128.html\">u128</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.usize.html\">usize</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_int_static/struct.ObjStaticInt.html\" title=\"struct debris_llir::objects::obj_int_static::ObjStaticInt\">ObjStaticInt</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.unit.html\">()</a>&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_null/struct.ObjNull.html\" title=\"struct debris_llir::objects::obj_null::ObjNull\">ObjNull</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.str.html\">str</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_string/struct.ObjString.html\" title=\"struct debris_llir::objects::obj_string::ObjString\">ObjString</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"struct\" href=\"debris_llir/class/struct.Class.html\" title=\"struct debris_llir::class::Class\">Class</a>&gt;, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_llir/objects/obj_tuple_object/struct.Tuple.html\" title=\"struct debris_llir::objects::obj_tuple_object::Tuple\">Tuple</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;&amp;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/std/collections/hash/map/struct.HashMap.html\" title=\"struct std::collections::hash::map::HashMap\">HashMap</a>&lt;<a class=\"struct\" href=\"debris_llir/block_id/struct.BlockId.html\" title=\"struct debris_llir::block_id::BlockId\">BlockId</a>, <a class=\"struct\" href=\"debris_llir/llir_nodes/struct.Function.html\" title=\"struct debris_llir::llir_nodes::Function\">Function</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/core/hash/struct.BuildHasherDefault.html\" title=\"struct core::hash::BuildHasherDefault\">BuildHasherDefault</a>&lt;FxHasher&gt;&gt;&gt; for <a class=\"struct\" href=\"debris_llir/opt/call_graph/struct.CallGraph.html\" title=\"struct debris_llir::opt::call_graph::CallGraph\">CallGraph</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.i32.html\">i32</a>&gt; for <a class=\"struct\" href=\"debris_llir/opt/optimizers/arithmetic_optimizer/struct.Fraction.html\" title=\"struct debris_llir::opt::optimizers::arithmetic_optimizer::Fraction\">Fraction</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/nightly/std/primitive.bool.html\">bool</a>&gt; for <a class=\"enum\" href=\"debris_llir/opt/optimizers/redundancy_optimizer/enum.SimplifiedCondition.html\" title=\"enum debris_llir::opt::optimizers::redundancy_optimizer::SimplifiedCondition\">SimplifiedCondition</a>"]],
"debris_mir":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_mir/mir_context/enum.ReturnContext.html\" title=\"enum debris_mir::mir_context::ReturnContext\">ReturnContext</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_builder/enum.ReturnContextBehavior.html\" title=\"enum debris_mir::mir_builder::ReturnContextBehavior\">ReturnContextBehavior</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.Branch.html\" title=\"struct debris_mir::mir_nodes::Branch\">Branch</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.FunctionCall.html\" title=\"struct debris_mir::mir_nodes::FunctionCall\">FunctionCall</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.Goto.html\" title=\"struct debris_mir::mir_nodes::Goto\">Goto</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.RuntimePromotion.html\" title=\"struct debris_mir::mir_nodes::RuntimePromotion\">RuntimePromotion</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.RuntimeCopy.html\" title=\"struct debris_mir::mir_nodes::RuntimeCopy\">RuntimeCopy</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VerifyValueComptime.html\" title=\"struct debris_mir::mir_nodes::VerifyValueComptime\">VerifyValueComptime</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VerifyTupleLength.html\" title=\"struct debris_mir::mir_nodes::VerifyTupleLength\">VerifyTupleLength</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VerifyPropertyExists.html\" title=\"struct debris_mir::mir_nodes::VerifyPropertyExists\">VerifyPropertyExists</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.PrimitiveDeclaration.html\" title=\"struct debris_mir::mir_nodes::PrimitiveDeclaration\">PrimitiveDeclaration</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.VariableUpdate.html\" title=\"struct debris_mir::mir_nodes::VariableUpdate\">VariableUpdate</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.PropertyUpdate.html\" title=\"struct debris_mir::mir_nodes::PropertyUpdate\">PropertyUpdate</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_mir/mir_nodes/struct.PropertyAccess.html\" title=\"struct debris_mir::mir_nodes::PropertyAccess\">PropertyAccess</a>&gt; for <a class=\"enum\" href=\"debris_mir/mir_nodes/enum.MirNode.html\" title=\"enum debris_mir::mir_nodes::MirNode\">MirNode</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/vec/struct.Vec.html\" title=\"struct alloc::vec::Vec\">Vec</a>&lt;<a class=\"enum\" href=\"debris_mir/mir_primitives/enum.MirFormatStringComponent.html\" title=\"enum debris_mir::mir_primitives::MirFormatStringComponent\">MirFormatStringComponent</a>, <a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/alloc/struct.Global.html\" title=\"struct alloc::alloc::Global\">Global</a>&gt;&gt; for <a class=\"struct\" href=\"debris_mir/mir_primitives/struct.MirFormatString.html\" title=\"struct debris_mir::mir_primitives::MirFormatString\">MirFormatString</a>"]],
"debris_parser":[["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/nightly/alloc/rc/struct.Rc.html\" title=\"struct alloc::rc::Rc\">Rc</a>&lt;<a class=\"struct\" href=\"debris_parser/syntax_tree/struct.SyntaxTree.html\" title=\"struct debris_parser::syntax_tree::SyntaxTree\">SyntaxTree</a>&gt;&gt; for <a class=\"struct\" href=\"debris_parser/ast/struct.Ast.html\" title=\"struct debris_parser::ast::Ast\">Ast</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_parser/token/struct.Token.html\" title=\"struct debris_parser::token::Token\">Token</a>&gt; for <a class=\"enum\" href=\"debris_parser/ast/enum.AstNodeOrToken.html\" title=\"enum debris_parser::ast::AstNodeOrToken\">AstNodeOrToken</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_parser/ast/struct.AstNode.html\" title=\"struct debris_parser::ast::AstNode\">AstNode</a>&gt; for <a class=\"enum\" href=\"debris_parser/ast/enum.AstNodeOrToken.html\" title=\"enum debris_parser::ast::AstNodeOrToken\">AstNodeOrToken</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"enum\" href=\"debris_parser/token/enum.TokenKind.html\" title=\"enum debris_parser::token::TokenKind\">TokenKind</a>&gt; for <a class=\"enum\" href=\"debris_parser/error/enum.ExpectedItem.html\" title=\"enum debris_parser::error::ExpectedItem\">ExpectedItem</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_parser/node/struct.NodeId.html\" title=\"struct debris_parser::node::NodeId\">NodeId</a>&gt; for <a class=\"enum\" href=\"debris_parser/node/enum.NodeChild.html\" title=\"enum debris_parser::node::NodeChild\">NodeChild</a>"],["impl <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/convert/trait.From.html\" title=\"trait core::convert::From\">From</a>&lt;<a class=\"struct\" href=\"debris_parser/token/struct.Token.html\" title=\"struct debris_parser::token::Token\">Token</a>&gt; for <a class=\"enum\" href=\"debris_parser/node/enum.NodeChild.html\" title=\"enum debris_parser::node::NodeChild\">NodeChild</a>"]]
};if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()