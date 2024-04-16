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

mod model;

use clippy_utils::{
    def_path_def_ids, diagnostics::span_lint_and_sugg, path_to_local_id, source::snippet, ty::{is_copy, peel_mid_ty_refs}
};
use model::{Call, ItemFn, Method};
use rustc_data_structures::fx::FxHashSet;
use rustc_errors::Applicability;
use rustc_hir::{
    def_id::LocalDefId,
    intravisit::{walk_expr, FnKind, Visitor},
    BindingAnnotation, Body, ByRef, Expr, ExprKind, FnDecl, HirId, Local, Node, Pat, PatKind,
    QPath, Stmt, StmtKind, TyKind, UnOp,
};
use rustc_lint::{EarlyLintPass, LateContext, LateLintPass};
use rustc_middle::ty::{self, adjustment::Adjust};
use rustc_span::Span;
use std::{cmp::min, fmt::Write};

dylint_linting::impl_late_lint! {
    pub PROJECT_HELPER,
    Warn,
    "description ppp here",
    ProjectHelper::new()
}

pub struct ProjectHelper {}

impl ProjectHelper {
    pub fn new() -> Self {
        Self {}
    }
}

impl<'tcx> LateLintPass<'tcx> for ProjectHelper {
    fn check_fn(
        &mut self,
        cx: &LateContext<'tcx>,
        fn_kind: FnKind<'tcx>,
        _: &'tcx FnDecl<'tcx>,
        body: &'tcx Body<'tcx>,
        span: Span,
        lid: LocalDefId,
    ) {
        let (name, span) = match fn_kind {
            FnKind::Method(i, s) => (i.to_string(), format!("{:?}", span)),
            FnKind::ItemFn(i, g, h) => (i.to_string(), format!("{:?}", span)),
            FnKind::Closure => ("Closure".to_string(), format!("{:?}", span)),
        };
        let params = body
            .params
            .iter()
            .map(|param| {
                let name = snippet(cx, param.pat.span, "..").to_string();
                model::FnParam { name }
            })
            .collect::<Vec<_>>();
        let mut calls = Vec::new();
        scan_call_in_body(&mut calls, body);
        let fi = model::FnItem {
            name,
            span,
            params,
            calls,
        };
        let path: rustc_hir::def_id::DefId = lid.to_def_id();
        
        println!("Body {:#?}", body);
        println!("DefId {:#?}", path);
        println!("Fnitem {:#?}\n", fi);
    }
}

fn scan_call_in_stmt<'tcx>(calls: &mut Vec<Call>, stmt: &'tcx Stmt<'tcx>) {
    match &stmt.kind {
        StmtKind::Local(lo) => {
            scan_call_in_local(calls, lo);
        }
        _ => {}
    }
}

fn scan_call_in_local<'tcx>(calls: &mut Vec<Call>, local: &'tcx Local<'tcx>) {
    if let Some(expr) = local.init {
        scan_call_in_expr(calls, expr);
    }
}

fn get_call_in_call_expr<'tcx>(expr: &'tcx Expr<'tcx>) -> anyhow::Result<ItemFn> {
    let qpath = if let ExprKind::Path(pa) = &expr.kind {
        pa
    } else {
        return Err(anyhow::anyhow!("parse call expr error {:#?}", expr));
    };
    let item = get_call_in_qpath(qpath)?;
    Ok(item)
}

fn scan_call_in_expr<'tcx>(calls: &mut Vec<Call>, expr: &'tcx Expr<'tcx>) {
    match &expr.kind {
        ExprKind::Block(bl, _la) => {
            for stmt in bl.stmts {
                scan_call_in_stmt(calls, stmt);
            }
            if let Some(expr) = bl.expr {
                scan_call_in_expr(calls, expr);
            }
        }
        ExprKind::Call(call_expr, exprs) => {
            match get_call_in_call_expr(call_expr) {
                Ok(item) => {
                    calls.push(Call {
                        inner: model::CallInner::ItemFn(item),
                        useful: false,
                        scan_status: true,
                    });
                }
                Err(e) => {
                    calls.push(Call {
                        inner: model::CallInner::Closure(format!("{:?}",call_expr.span)),
                        useful: false,
                        scan_status: true,
                    });
                }
            };
            exprs
                .iter()
                .for_each(|expr_c| scan_call_in_expr(calls, expr_c));
        }
        _ => {}
    }
}

fn get_struct_in_qpath<'tcx>(qpath: &'tcx QPath<'tcx>) -> anyhow::Result<String> {
    let pa = match &qpath {
        QPath::Resolved(_ty, pa) => pa,
        _ => {
            return Err(anyhow::anyhow!("parse struct path error {:#?}", qpath));
        }
    };
    let def = match pa.res {
        rustc_hir::def::Res::Def(_, def_id) => format!("{:#?}", def_id),
        _ => {
            return Err(anyhow::anyhow!("parse struct path error {:#?}", qpath));
        }
    };

    Ok(def)
}

fn get_call_in_qpath<'tcx>(qpath: &'tcx QPath<'tcx>) -> anyhow::Result<ItemFn> {
    let ans = match &qpath {
        QPath::TypeRelative(ty, ps) => match &ty.kind {
            TyKind::Path(qpath) => {
                let rel_struct = match get_struct_in_qpath(qpath) {
                    Ok(def) => def,
                    Err(e) => {
                        println!("get_call_in_qpath {:#?} {:#?}", qpath, e);
                        "unknow".to_string()
                    }
                };
                let name = ps.ident.name.to_string();
                let span = format!("{:?}", ps.ident.span);
                ItemFn {
                    name,
                    span,
                    rel_struct,
                }
            }
            _ => {
                return Err(anyhow::anyhow!("parse call path error {:#?}", qpath));
            }
        },
        _ => {
            return Err(anyhow::anyhow!("parse call path error {:#?}", qpath));
        }
    };

    Ok(ans)
}

fn scan_call_in_body<'tcx>(calls: &mut Vec<Call>, body: &'tcx Body<'tcx>) {
    scan_call_in_expr(calls, body.value);
}
