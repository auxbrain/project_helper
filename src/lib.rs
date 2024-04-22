#![feature(rustc_private)]
#![feature(let_chains)]
#![warn(unused_extern_crates)]

extern crate rustc_hir;
extern crate rustc_span;
// extern crate rustc_data_structures;
// extern crate rustc_errors;
// extern crate rustc_middle;
// extern crate rustc_hir_pretty;
// extern crate rustc_index;
// extern crate rustc_infer;
// extern crate rustc_lexer;
// extern crate rustc_arena;
// extern crate rustc_ast;
// extern crate rustc_ast_pretty;
// extern crate rustc_attr;
// extern crate rustc_mir_dataflow;
// extern crate rustc_parse;
// extern crate rustc_target;
// extern crate rustc_trait_selection;

pub mod model;
pub mod stroage;

use clippy_utils::source::snippet;
use model::{Call, ItemFnCall, MethodCall};
use rustc_hir::{def::Res, Ty};
use rustc_hir::{
    def_id::LocalDefId, intravisit::FnKind, Body, Expr, ExprKind, FnDecl, HirId, Local, QPath,
    Stmt, StmtKind, TyKind,
};
use rustc_lint::{LateContext, LateLintPass};
use rustc_span::Span;

use crate::stroage::GLOBAL_INFO;

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
        dec: &'tcx FnDecl<'tcx>,
        body: &'tcx Body<'tcx>,
        span: Span,
        lid: LocalDefId,
    ) {
        let (name, span) = match fn_kind {
            FnKind::Method(i, _s) => (i.to_string(), format!("{:?}", span)),
            FnKind::ItemFn(i, _g, _h) => (i.to_string(), format!("{:?}", span)),
            FnKind::Closure => ("Closure".to_string(), format!("{:?}", span)),
        };
        let params = get_type_by_desc(cx, dec, body);
        let mut calls = Vec::new();
        scan_call_in_body(cx, &mut calls, body);
        let def_id = format!("{:?}", lid.to_def_id());
        let fi = model::FnItem {
            name,
            span,
            params,
            calls,
            desp: "".to_string(),
            def_id: def_id.clone(),
        };
        let jfi = serde_json::to_string(&fi).unwrap();
        let key = format!("fn_{}", def_id);
        GLOBAL_INFO.insert(key, jfi.as_bytes()).unwrap();
        GLOBAL_INFO.flush().unwrap();
    }
}

fn get_type_by_desc<'tcx>(
    cx: &LateContext<'tcx>,
    dec: &'tcx FnDecl<'tcx>,
    body: &'tcx Body<'tcx>,
) -> Vec<model::FnParam> {
    let param_tys = dec
        .inputs
        .iter()
        .map(|v| get_rel_struct_in_ty(v).unwrap_or_else(|e| format!("{:?}", e)))
        .collect::<Vec<_>>();
    let params = body
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let name = snippet(cx, param.pat.span, "..").to_string();
            model::FnParam {
                name,
                ty: param_tys
                    .get(index)
                    .cloned()
                    .unwrap_or("unknown".to_string()),
                desp: "".to_string(),
            }
        })
        .collect::<Vec<_>>();
    params
}

fn scan_call_in_stmt<'tcx>(cx: &LateContext<'tcx>, calls: &mut Vec<Call>, stmt: &'tcx Stmt<'tcx>) {
    match &stmt.kind {
        StmtKind::Local(lo) => {
            scan_call_in_local(cx, calls, lo);
        }
        StmtKind::Semi(expr) => {
            scan_call_in_expr(cx, calls, expr);
        }
        _ => {}
    }
}

fn scan_call_in_local<'tcx>(
    cx: &LateContext<'tcx>,
    calls: &mut Vec<Call>,
    local: &'tcx Local<'tcx>,
) {
    if let Some(expr) = local.init {
        scan_call_in_expr(cx, calls, expr);
    }
}

fn get_call_in_call_expr_inner<'tcx>(
    cx: &LateContext<'tcx>,
    expr: &'tcx Expr<'tcx>,
) -> anyhow::Result<ItemFnCall> {
    let qpath = if let ExprKind::Path(pa) = &expr.kind {
        pa
    } else {
        return Err(anyhow::anyhow!("parse call expr error {:?}", expr));
    };
    let hir_id = expr.hir_id;
    let item = get_call_in_qpath(cx, qpath, hir_id)?;
    Ok(item)
}

fn scan_call_in_expr<'tcx>(cx: &LateContext<'tcx>, calls: &mut Vec<Call>, expr: &'tcx Expr<'tcx>) {
    match &expr.kind {
        ExprKind::Block(bl, _la) => {
            for stmt in bl.stmts {
                scan_call_in_stmt(cx, calls, stmt);
            }
            if let Some(expr) = bl.expr {
                scan_call_in_expr(cx, calls, expr);
            }
        }
        ExprKind::Call(call_expr, exprs) => {
            exprs
                .iter()
                .for_each(|expr_c| scan_call_in_expr(cx, calls, expr_c));
            get_call_in_call_expr(cx, calls, call_expr);
        }
        ExprKind::MethodCall(ps, ex, exs, _sp) => {
            scan_call_in_expr(cx, calls, ex);
            exs.iter()
                .for_each(|expr_c| scan_call_in_expr(cx, calls, expr_c));
            get_method_call_in_call_expr(cx, calls, expr, ps);
        }
        ExprKind::Closure(cl) => {
            calls.push(Call {
                desp: "".to_string(),
                def_id: format!("{:?}", cl.def_id),
                inner: model::CallInner::Closure(format!("{:?}", cl.fn_decl_span)),
                useful: false,
                scan_status: false,
            });
        }
        _ => {}
    }
}

fn get_method_call_in_call_expr<'tcx>(
    cx: &LateContext<'tcx>,
    calls: &mut Vec<Call>,
    expr: &'tcx Expr<'tcx>,
    ps: &rustc_hir::PathSegment<'tcx>,
) {
    match get_method_call_in_call_expr_inner(cx, expr.hir_id, ps) {
        Ok(item) => {
            calls.push(Call {
                desp: "".to_string(),
                def_id: item.def_id.clone(),
                inner: model::CallInner::Method(item),
                useful: false,
                scan_status: true,
            });
        }
        Err(e) => {
            calls.push(Call {
                desp: "".to_string(),
                def_id: "".to_string(),
                inner: model::CallInner::Closure(format!("{:?} {:?}", ps.ident, e)),
                useful: false,
                scan_status: false,
            });
        }
    };
}

fn get_call_in_call_expr<'tcx>(
    cx: &LateContext<'tcx>,
    calls: &mut Vec<Call>,
    call_expr: &'tcx Expr<'tcx>,
) {
    match get_call_in_call_expr_inner(cx, call_expr) {
        Ok(item) => {
            calls.push(Call {
                desp: "".to_string(),
                def_id: item.def_id.clone(),
                inner: model::CallInner::ItemFn(item),
                useful: false,
                scan_status: true,
            });
        }
        Err(e) => {
            calls.push(Call {
                desp: "".to_string(),
                def_id: "".to_string(),
                inner: model::CallInner::Closure(format!("{:?} {:?}", call_expr.span, e)),
                useful: false,
                scan_status: false,
            });
        }
    };
}

fn get_def_id_by_res(res: Res) -> anyhow::Result<String> {
    let def_id = match res {
        Res::Def(_kd, id) => {
            format!("{:?}", id)
        }
        Res::Local(hir_id) => {
            let id = hir_id.owner.to_def_id();
            format!("{:?}", id)
        }
        _ => {
            return Err(anyhow::anyhow!("get_def_id_by_res {:?}", res));
        }
    };
    Ok(def_id)
}

fn get_def_id_by_hir_id<'tcx>(cx: &LateContext<'tcx>, id: HirId) -> anyhow::Result<String> {
    let res: Res<HirId> = cx
        .maybe_typeck_results()
        .filter(|typeck_results| typeck_results.hir_owner == id.owner)
        .or_else(|| {
            cx.tcx
                .has_typeck_results(id.owner.to_def_id())
                .then(|| cx.tcx.typeck(id.owner.def_id))
        })
        .and_then(|typeck_results| typeck_results.type_dependent_def(id))
        .map_or(Res::Err, |(kind, def_id)| Res::Def(kind, def_id));
    let def_id = get_def_id_by_res(res)?;

    Ok(def_id)
}

fn get_method_call_in_call_expr_inner<'tcx>(
    cx: &LateContext<'tcx>,
    hir_id: HirId,
    ps: &rustc_hir::PathSegment<'tcx>,
) -> anyhow::Result<MethodCall> {
    let def_id = get_def_id_by_hir_id(cx, hir_id)?;
    let name = ps.ident.name.to_string();
    Ok(MethodCall { name, def_id })
}

fn get_rel_struct_in_ty<'tcx>(ty: &Ty<'tcx>) -> anyhow::Result<String> {
    let qpath = if let TyKind::Path(qpath) = &ty.kind {
        qpath
    } else {
        return Err(anyhow::anyhow!("get_rel_struct_in_ty 1  {:?}", ty));
    };
    let reso = if let QPath::Resolved(_ty, pa) = qpath {
        pa
    } else {
        return Err(anyhow::anyhow!("get_rel_struct_in_ty 2 {:?}", ty));
    };
    let def_id = get_def_id_by_res(reso.res)?;

    Ok(def_id)
}

fn get_call_in_qpath<'tcx>(
    cx: &LateContext<'tcx>,
    qpath: &'tcx QPath<'tcx>,
    hir_id: HirId,
) -> anyhow::Result<ItemFnCall> {
    let ans = match &qpath {
        QPath::TypeRelative(ty, ps) => {
            let rel_struct = get_rel_struct_in_ty(ty)?;
            let def_id = get_def_id_by_hir_id(cx, hir_id)?;
            let name = ps.ident.name.to_string();
            ItemFnCall {
                name,
                def_id,
                rel_struct,
            }
        }
        QPath::Resolved(_ty, pa) => {
            let def_id = get_def_id_by_res(pa.res)?;
            let name = "".to_string();
            let rel_struct = "".to_string();
            ItemFnCall {
                name,
                def_id,
                rel_struct,
            }
        }
        _ => {
            return Err(anyhow::anyhow!("parse call path error {:?}", qpath));
        }
    };

    Ok(ans)
}

fn scan_call_in_body<'tcx>(cx: &LateContext<'tcx>, calls: &mut Vec<Call>, body: &'tcx Body<'tcx>) {
    scan_call_in_expr(cx, calls, body.value);
}
