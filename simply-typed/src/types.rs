use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Nat,
    Con(Box<Type>, Box<Type>),
    List(Box<Type>),
    Unit,
    Variant(Vec<(String, Type)>),
}

pub fn con(t1: &Type, t2: &Type) -> Type {
    Type::Con(box t1.clone(), box t2.clone())
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "𝔹"),
            Type::Nat => write!(f, "ℕ"),
            Type::Con(l, r) => write!(f, "({} → {})", l, r),
            Type::List(dtype) => write!(f, "List[{}]", dtype),
            Type::Unit => write!(f, "Unit"),
            Type::Variant(vec) => {
                write!(f, "<")?;
                let len = vec.len();

                for (i, (name, dtype)) in vec.iter().enumerate() {
                    write!(f, "{name}:{dtype}")?;
                    if i + 1 < len {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ">")
            }
        }
    }
}
