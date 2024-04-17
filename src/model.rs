use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FnItem {
    pub name: String,
    pub span: String,
    pub def_id: String,
    pub params: Vec<FnParam>,
    pub calls: Vec<Call>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FnParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum CallInner {
    ItemFn(ItemFnCall),
    Method(MethodCall),
    Closure(String),
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct ItemFnCall {
    pub name: String,
    pub def_id: String,
    pub rel_struct: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct MethodCall {
    pub name: String,
    pub def_id: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Call {
    pub def_id: String,
    pub inner: CallInner,
    pub useful: bool,
    pub scan_status: bool,
}
