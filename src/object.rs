use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum ObjectType {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectType::Null => f.write_str("NULL"),
            ObjectType::Boolean(_v) => write!(f, "BOOLEAN"),
            ObjectType::Integer(_v) => write!(f, "INTEGER"),
        }
    }
}
/// Objects are the internal represenation of values in evaluation
#[derive(Debug, Clone)]
pub struct Object {
    o_type: ObjectType,
}

impl Object {
    pub fn with_type(o_type: ObjectType) -> Self {
        Self { o_type }
    }

    pub fn get_type(&self) -> ObjectType {
        self.o_type.clone()
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.get_type() {
            ObjectType::Null => f.write_str("null"),
            ObjectType::Boolean(v) => write!(f, "{}", v),
            ObjectType::Integer(v) => write!(f, "{}", v),
        }
    }
}

pub const TRUE: Object = Object {
    o_type: ObjectType::Boolean(true),
};
pub const FALSE: Object = Object {
    o_type: ObjectType::Boolean(false),
};

pub const NULL: Object = Object {
    o_type: ObjectType::Null,
};
