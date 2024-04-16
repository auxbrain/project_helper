use serde::{Serialize,Deserialize};


#[derive(Debug,Serialize,Deserialize,Clone)]
pub struct FnItem {
    pub name: String,
    pub span: String,
    pub params: Vec<FnParam>,
    pub calls: Vec<Call>,
}

#[derive(Debug,Serialize,Deserialize,Clone)]
pub struct FnParam {
    pub name: String,
}

#[derive(Debug,Serialize,Deserialize,Clone)]
pub enum CallInner {
    ItemFn(ItemFn),
    Method(Method),
    Closure(String)
}

#[derive(Debug,Serialize,Deserialize,Clone,Default)]
pub struct ItemFn {
    pub name: String,
    pub span: String,
    pub rel_struct: String,
}

#[derive(Debug,Serialize,Deserialize,Clone,Default)]
pub struct Method{
    pub name: String,
    pub span: String,
    pub struct_def: String,
}

#[derive(Debug,Serialize,Deserialize,Clone)]
pub struct Call {
    pub inner: CallInner,
    pub useful: bool,
    pub scan_status: bool,
}