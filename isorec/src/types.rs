use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Nat,
    Con(Box<Type>, Box<Type>),
    Unit,
    Variant(Vec<(String, Type)>),
    Record(Vec<(String, Type)>),
    Rec(String, Box<Type>),
    TyVar(String),
}

impl Type {
    pub fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Rec(x1, _), Type::TyVar(x2)) | (Type::TyVar(x2), Type::Rec(x1, _)) => x1 == x2,
            _ => self == other,
        }
    }

    pub fn sub(&self, rname: &String, new_type: &Type) -> Type {
        match self {
            Type::TyVar(tyname) if rname == tyname => new_type.clone(),
            Type::Con(ty1, ty2) => {
                Type::Con(box ty1.sub(rname, new_type), box ty2.sub(rname, new_type))
            }
            Type::Record(vec) => Type::Record(
                vec.iter()
                    .map(|(name, dtype)| (name.clone(), dtype.sub(name, new_type)))
                    .collect(),
            ),
            Type::Variant(vec) => Type::Variant(
                vec.iter()
                    .map(|(name, dtype)| (name.clone(), dtype.sub(name, new_type)))
                    .collect(),
            ),
            Type::Bool | Type::Nat | Type::Unit => self.clone(),
            _ => self.clone(),
        }
    }

    pub fn unsub(&self, rtype: &Type, new_name: &String) -> Type {
        match self {
            _ if self == rtype => Type::TyVar(new_name.into()),
            Type::Con(ty1, ty2) => Type::Con(
                box ty1.unsub(rtype, new_name),
                box ty2.unsub(rtype, new_name),
            ),
            Type::Record(vec) => Type::Record(
                vec.iter()
                    .map(|(name, dtype)| (name.clone(), dtype.unsub(rtype, new_name)))
                    .collect(),
            ),
            Type::Variant(vec) => Type::Variant(
                vec.iter()
                    .map(|(name, dtype)| (name.clone(), dtype.unsub(rtype, new_name)))
                    .collect(),
            ),
            Type::Bool | Type::Nat | Type::Unit => self.clone(),
            _ => self.clone(),
        }
    }
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
            Type::Record(vec) => {
                write!(f, "{{")?;
                let len = vec.len();

                for (i, (name, dtype)) in vec.iter().enumerate() {
                    write!(f, "{name}:{dtype}")?;
                    if i + 1 < len {
                        write!(f, ", ")?;
                    }
                }

                write!(f, "}}")
            }
            Type::Rec(x, dtype) => write!(f, "μ{x} . {dtype}"),
            Type::TyVar(name) => write!(f, "{name}"),
        }
    }
}
