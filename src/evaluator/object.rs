use std::fmt;
use std::fmt::Formatter;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Null => write!(f, "Null"),
            Object::Int(value) => write!(f, "{}", value),
            Object::Float(value) => write!(f, "{}", value),
            Object::Bool(value) => write!(f, "{}", value),
            Object::String(value) => write!(f, "{}", value),
            Object::Return(value) => write!(f, "{}", value),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum ObjType {
    Null,
    Int,
    Float,
    Bool,
    String,
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ObjType::Null => write!(f, "null"),
            ObjType::Int => write!(f, "int"),
            ObjType::Float => write!(f, "float"),
            ObjType::Bool => write!(f, "bool"),
            ObjType::String => write!(f, "string"),
        }
    }
}

impl Object {
    pub fn typ(&mut self) -> ObjType {
        match self {
            Object::Null => ObjType::Null,
            Object::Int(_) => ObjType::Int,
            Object::Float(_) => ObjType::Float,
            Object::Bool(_) => ObjType::Bool,
            Object::String(_) => ObjType::String,
            Object::Return(obj) => obj.typ(),
        }
    }
}
