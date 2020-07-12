#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ObjectType {
    Integer(i64),
    Boolean(bool),
    Null,
}

/// Objects are the internal represenation of values in evaluation
#[derive(Debug)]
pub struct Object {
    o_type: ObjectType,
}

impl Object {

    pub fn with_type(o_type: ObjectType) -> Self {
        Self {
            o_type
        }
    }

    pub fn get_type(&self) -> ObjectType {
        self.o_type
    }

    pub fn inspect(&self) -> String {
        match &self.o_type {
            ObjectType::Integer(v) => format!("{}", v),
            ObjectType::Boolean(b) => format!("{}", b),
            ObjectType::Null => String::from("null"),
        }
    }
}
