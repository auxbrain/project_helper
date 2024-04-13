#![feature(rustc_private)]
#![feature(let_chains)]
#![warn(unused_extern_crates)]

extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_attr;
extern crate rustc_data_structures;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_lexer;
extern crate rustc_middle;
extern crate rustc_mir_dataflow;
extern crate rustc_parse;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;

use clippy_utils::{
    diagnostics::span_lint_and_sugg,
    path_to_local_id,
    source::snippet,
    ty::{is_copy, peel_mid_ty_refs},
};
use rustc_data_structures::fx::FxHashSet;
use rustc_errors::Applicability;
use rustc_hir::{
    def_id::LocalDefId,
    intravisit::{walk_expr, FnKind, Visitor},
    BindingAnnotation, Body, ByRef, Expr, ExprKind, FnDecl, HirId, Node, Pat, PatKind, UnOp,
};
use rustc_lint::{LateContext, LateLintPass,EarlyLintPass};
use rustc_middle::ty::{self, adjustment::Adjust};
use rustc_span::Span;
use std::{cmp::min, fmt::Write};

dylint_linting::impl_late_lint! {
    pub PROJECT_HELPER,
    Warn,
    "description ppp here",
    ProjectHelper::new()
}

pub struct ProjectHelper{}

impl ProjectHelper {
    pub fn new() -> Self {
        Self {}
    }
}

pub struct FnItem{
    pub name: String,
    pub span: String,
}

impl<'tcx> LateLintPass<'tcx> for ProjectHelper {
    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        fn_kind: FnKind<'tcx>,
        _: &'tcx FnDecl<'tcx>,
        body: &'tcx Body<'tcx>,
        span: Span,
        _: LocalDefId,
    ) {
        println!("Body {:?}",body);
        match fn_kind {
            FnKind::Method(i,g) => {
                println!("Method: {:?} {:?}", i.name,i.span);
                let inputs = g.decl.inputs;
                for i in inputs {
                    println!("Input: {:?}",i);
                }
                println!("Output: {:?}",g.decl.output);
            }
            _ => {
                
            }
        }
        let a = { params: [Param { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).1), pat: Pat { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).2), kind: Binding(BindingAnnotation(No, Not), HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).2), self#0, None), span: src\main.rs:3:11: 3:15 (#0), default_binding_modes: true }, ty_span: src\main.rs:3:11: 3:15 (#0), span: src\main.rs:3:11: 3:15 (#0) }, Param { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).3), pat: Pat { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).4), kind: Binding(BindingAnnotation(No, Not), HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).4), a#0, None), span: src\main.rs:3:17: 3:18 (#0), default_binding_modes: true }, ty_span: src\main.rs:3:20: 3:23 (#0), span: src\main.rs:3:17: 3:23 (#0) }, Param { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).5), pat: Pat { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).6), kind: Binding(BindingAnnotation(No, Not), HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).6), b#0, None), span: src\main.rs:3:25: 3:26 (#0), default_binding_modes: true }, ty_span: src\main.rs:3:28: 3:31 (#0), span: src\main.rs:3:25: 3:31 (#0) }, Param { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).7), pat: Pat { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).8), kind: Binding(BindingAnnotation(No, Not), HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).8), c#0, None), span: src\main.rs:3:33: 3:34 (#0), default_binding_modes: true }, ty_span: src\main.rs:3:36: 3:37 (#0), span: src\main.rs:3:33: 3:37 (#0) }], value: Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).25), kind: Block(Block { stmts: [Stmt { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).9), kind: Local(Local { pat: Pat { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).16), kind: Binding(BindingAnnotation(No, Not), HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).16), p#0, None), span: src\main.rs:4:13: 4:14 (#0), default_binding_modes: true }, ty: None, init: Some(Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).10), kind: Binary(Spanned { node: Add, span: src\main.rs:4:19: 4:20 (#0) }, Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).11), kind: Path(Resolved(None, Path { span: src\main.rs:4:17: 4:18 (#0), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).4)), segments: [PathSegment { ident: a#0, hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).12), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).4)), args: None, infer_args: true }] })), span: src\main.rs:4:17: 4:18 (#0) }, Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).13), kind: Path(Resolved(None, Path { span: src\main.rs:4:21: 4:22 (#0), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).6)), segments: [PathSegment { ident: b#0, hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).14), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).6)), args: None, infer_args: true }] })), span: src\main.rs:4:21: 4:22 (#0) }), span: src\main.rs:4:17: 4:22 (#0) }), els: None, hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).15), span: src\main.rs:4:9: 4:23 (#0), source: Normal }), span: src\main.rs:4:9: 4:23 (#0) }, Stmt { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).23), kind: Semi(Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).17), kind: Ret(Some(Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).18), kind: Binary(Spanned { node: Add, span: src\main.rs:5:18: 5:19 (#0) }, Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).19), kind: Path(Resolved(None, Path { span: src\main.rs:5:16: 5:17 (#0), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).16)), segments: [PathSegment { ident: p#0, hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).20), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).16)), args: None, infer_args: true }] })), span: src\main.rs:5:16: 5:17 (#0) }, Expr { hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).21), kind: Path(Resolved(None, Path { span: src\main.rs:5:20: 5:21 (#0), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).6)), segments: [PathSegment { ident: b#0, hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).22), res: Local(HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).6)), args: None, infer_args: true }] })), span: src\main.rs:5:20: 5:21 (#0) }), span: src\main.rs:5:16: 5:21 (#0) })), span: src\main.rs:5:9: 5:21 (#0) }), span: src\main.rs:5:9: 5:22 (#0) }], expr: None, hir_id: HirId(DefId(0:5 ~ p1[d850]::{impl#0}::aa).24), rules: DefaultBlock, span: src\main.rs:3:46: 6:6 (#0), targeted_by_break: false }, None), span: src\main.rs:3:46: 6:6 (#0) } }
    }
}

#[test]
fn ui() {
    dylint_testing::ui_test(
        env!("CARGO_PKG_NAME"),
        &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("ui"),
    );
}
