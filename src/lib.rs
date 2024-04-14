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
        println!("Body {:#?}",body);
        match fn_kind {
            FnKind::Method(i,g) => {
                println!("Method: {:#?} {:#?}", i.name,i.span);
                let inputs = g.decl.inputs;
                for i in inputs {
                    println!("Input: {:#?}",i);
                }
                println!("Output: {:#?}",g.decl.output);
            }
            _ => {
                
            }
        }
    }
}

#[test]
fn ui() {
    dylint_testing::ui_test(
        env!("CARGO_PKG_NAME"),
        &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("ui"),
    );
}
