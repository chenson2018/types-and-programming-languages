use crate::types::Type;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Var(String),
    Abs(String, Type, Box<Term>),
    App(Box<Term>, Box<Term>),
    True,
    False,
    Zero,
    Succ(Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
    Fix(Box<Term>),
    Nil(Type),
    Cons(Box<Term>, Box<Term>),
    IsNil(Box<Term>),
    Head(Box<Term>),
    Tail(Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TermAnon {
    Var(usize),
    Abs(Box<TermAnon>),
    App(Box<TermAnon>, Box<TermAnon>),
    True,
    False,
    Zero,
    Succ(Box<TermAnon>),
    If(Box<TermAnon>, Box<TermAnon>, Box<TermAnon>),
    Pred(Box<TermAnon>),
    IsZero(Box<TermAnon>),
    Fix(Box<TermAnon>),
    Nil,
    Cons(Box<TermAnon>, Box<TermAnon>),
    IsNil(Box<TermAnon>),
    Head(Box<TermAnon>),
    Tail(Box<TermAnon>),
}

impl From<usize> for Term {
    fn from(value: usize) -> Term {
        let mut term = Term::Zero;

        for _ in 0..value {
            term = Term::Succ(box term);
        }

        term
    }
}

impl TryFrom<&Term> for usize {
    type Error = &'static str;

    fn try_from(t: &Term) -> Result<Self, Self::Error> {
        match t {
            Term::Succ(_) => {
                let mut term = t.clone();
                let mut count = 0;

                while term != Term::Zero {
                    term = match term {
                        Term::Succ(t) => *t,
                        _ => return Err("nan"),
                    };
                    count += 1;
                }
                Ok(count)
            }
            Term::Zero => Ok(0),
            _ => Err("nan"),
        }
    }
}

impl From<usize> for TermAnon {
    fn from(value: usize) -> TermAnon {
        let mut term = TermAnon::Zero;

        for _ in 0..value {
            term = TermAnon::Succ(box term);
        }

        term
    }
}

impl TryFrom<&TermAnon> for usize {
    type Error = &'static str;

    fn try_from(t: &TermAnon) -> Result<Self, Self::Error> {
        match t {
            TermAnon::Succ(_) => {
                let mut term = t.clone();
                let mut count = 0;

                while term != TermAnon::Zero {
                    term = match term {
                        TermAnon::Succ(t) => *t,
                        _ => return Err("nan"),
                    };
                    count += 1;
                }
                Ok(count)
            }
            TermAnon::Zero => Ok(0),
            _ => Err("nan"),
        }
    }
}

// this is primarily for testing purposes
impl FromStr for Term {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut scanner = crate::scanner::Scanner::new(&s);
        if let Err(e) = scanner.scan() {
            return Err(e.clone());
        };
        let mut parser = crate::parser::Parser::from(&scanner);

        match parser.parse() {
            Err(e) => Err(e.to_string()),
            Ok(ast) => Ok(ast),
        }
    }
}

impl Display for TermAnon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ if self == &Term::and().to_anon() => write!(f, "and"),
            _ if self == &Term::not().to_anon() => write!(f, "not"),
            _ if self == &Term::or().to_anon() => write!(f, "or"),
            Self::Var(name) => write!(f, "{}", name),
            Self::App(left, right) => write!(f, "({} {})", left, right),
            Self::Abs(body) => write!(f, "(λ. {})", body),
            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Zero => write!(f, "nat_0"),
            Self::Succ(term) => match usize::try_from(self) {
                Ok(i) => write!(f, "nat_{i}"),
                Err(_) => write!(f, "succ({})", term),
            },
            Self::If(cond, consq, alt) => write!(f, "if {cond} then {consq} else {alt}"),
            Self::Pred(term) => write!(f, "pred({term})"),
            Self::IsZero(term) => write!(f, "iszero({term})"),
            Self::Fix(term) => write!(f, "fix({term})"),
            Self::Cons(..) => {
                write!(f, "[")?;
                let mut term = self.clone();
                loop {
                    match term {
                        TermAnon::Cons(t1, box TermAnon::Nil) => {
                            write!(f, "{}", t1)?;
                            break;
                        }
                        TermAnon::Cons(t1, t2) => {
                            write!(f, "{}, ", t1)?;
                            term = *t2;
                        }
                        _ => {
                            write!(f, "{}", term)?;
                            break;
                        }
                    };
                }
                write!(f, "]")
            }
            Self::Nil => write!(f, "[]"),
            Self::IsNil(t1) => write!(f, "isnil({t1})"),
            Self::Head(t1) => write!(f, "head({t1})"),
            Self::Tail(t1) => write!(f, "tail({t1})"),
        }
    }
}

// TODO nice printing, especially for adjacent names
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ if self.to_anon() == Term::and().to_anon() => write!(f, "and"),
            _ if self.to_anon() == Term::not().to_anon() => write!(f, "not"),
            _ if self.to_anon() == Term::or().to_anon() => write!(f, "or"),
            Self::Var(name) => write!(f, "{}", name),
            Self::App(left, right) => write!(f, "({} {})", left, right),
            Self::Abs(name, dtype, body) => write!(f, "(λ{}:{} . {})", name, dtype, body),
            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Zero => write!(f, "nat_0"),
            Self::Succ(term) => match usize::try_from(self) {
                Ok(i) => write!(f, "nat_{i}"),
                Err(_) => write!(f, "succ({})", term),
            },
            Self::If(cond, consq, alt) => write!(f, "if {cond} then {consq} else {alt}"),
            Self::Pred(term) => write!(f, "pred({term})"),
            Self::IsZero(term) => write!(f, "iszero({term})"),
            Self::Fix(term) => write!(f, "fix({term})"),
            Self::Cons(..) => {
                write!(f, "[")?;
                let mut term = self.clone();
                loop {
                    match term {
                        Term::Cons(t1, box Term::Nil(_)) => {
                            write!(f, "{}", t1)?;
                            break;
                        }
                        Term::Cons(t1, t2) => {
                            write!(f, "{}, ", t1)?;
                            term = *t2;
                        }
                        _ => {
                            write!(f, "{}", term)?;
                            break;
                        }
                    };
                }
                write!(f, "]")
            }
            Self::Nil(_) => write!(f, "[]"),
            Self::IsNil(t1) => write!(f, "isnil({t1})"),
            Self::Head(t1) => write!(f, "head({t1})"),
            Self::Tail(t1) => write!(f, "tail({t1})"),
        }
    }
}

pub fn var<T>(name: T) -> Term
where
    T: Into<String>,
{
    Term::Var(name.into())
}

pub fn abs<T>(name: T, t: Type, body: Term) -> Term
where
    T: Into<String>,
{
    Term::Abs(name.into(), t, box body)
}

pub fn app(l: Term, r: Term) -> Term {
    Term::App(box l, box r)
}

pub fn vara(name: usize) -> TermAnon {
    TermAnon::Var(name)
}

pub fn absa(body: TermAnon) -> TermAnon {
    TermAnon::Abs(box body)
}

pub fn appa(l: TermAnon, r: TermAnon) -> TermAnon {
    TermAnon::App(box l, box r)
}

impl TermAnon {
    pub fn shift(&self, d: isize) -> TermAnon {
        self.shift_cutoff(0, d)
    }

    fn shift_cutoff(&self, c: usize, d: isize) -> TermAnon {
        match self {
            TermAnon::Var(k) => {
                let shift = if k < &c {
                    *k
                } else {
                    ((*k as isize) + d) as usize
                };
                TermAnon::Var(shift)
            }
            TermAnon::Abs(t1) => {
                let t1_shift = t1.shift_cutoff(c + 1, d);
                TermAnon::Abs(box t1_shift)
            }
            Self::Fix(t1) => Self::Fix(box t1.shift_cutoff(c + 1, d)),
            TermAnon::App(t1, t2) => {
                TermAnon::App(box t1.shift_cutoff(c, d), box t2.shift_cutoff(c, d))
            }
            Self::If(cond, consq, alt) => Self::If(
                box cond.shift_cutoff(c, d),
                box consq.shift_cutoff(c, d),
                box alt.shift_cutoff(c, d),
            ),
            Self::Succ(t1) => Self::Succ(box t1.shift_cutoff(c, d)),
            Self::Pred(t1) => Self::Pred(box t1.shift_cutoff(c, d)),
            Self::IsZero(t1) => Self::IsZero(box t1.shift_cutoff(c, d)),
            TermAnon::False | TermAnon::True | TermAnon::Zero | TermAnon::Nil => self.clone(),
            TermAnon::Cons(t1, t2) => {
                TermAnon::Cons(box t1.shift_cutoff(c, d), box t2.shift_cutoff(c, d))
            }
            TermAnon::IsNil(t1) => TermAnon::IsNil(box t1.shift_cutoff(c, d)),
            TermAnon::Head(t1) => TermAnon::Head(box t1.shift_cutoff(c, d)),
            TermAnon::Tail(t1) => TermAnon::Tail(box t1.shift_cutoff(c, d)),
        }
    }

    pub fn sub(&self, j: usize, s: &TermAnon) -> TermAnon {
        match self {
            Self::Var(k) => {
                if k == &j {
                    s.clone()
                } else {
                    self.clone()
                }
            }
            Self::Abs(t1) => absa(t1.sub(j + 1, &s.shift(1))),
            Self::Fix(t1) => Self::Fix(box t1.sub(j + 1, &s.shift(1))),
            Self::App(t1, t2) => appa(t1.sub(j, s), t2.sub(j, s)),
            Self::Succ(t1) => Self::Succ(box t1.sub(j, s)),
            Self::IsZero(t1) => Self::IsZero(box t1.sub(j, s)),
            Self::Pred(t1) => Self::Pred(box t1.sub(j, s)),
            Self::If(cond, consq, alt) => {
                Self::If(box cond.sub(j, s), box consq.sub(j, s), box alt.sub(j, s))
            }
            TermAnon::False | TermAnon::True | TermAnon::Zero | TermAnon::Nil => self.clone(),
            TermAnon::Cons(t1, t2) => TermAnon::Cons(box t1.sub(j, s), box t2.sub(j, s)),
            TermAnon::IsNil(t1) => TermAnon::IsNil(box t1.sub(j, s)),
            TermAnon::Head(t1) => TermAnon::Head(box t1.sub(j, s)),
            TermAnon::Tail(t1) => TermAnon::Tail(box t1.sub(j, s)),
        }
    }

    pub fn full(&self) -> Self {
        match self {
            Self::App(box Self::Abs(t12), box t2) => t12.sub(0, &t2.shift(1)).shift(-1).full(),
            Self::Fix(box Self::Abs(t2)) => t2.sub(0, self),
            Self::App(t1, t2) => {
                let partial = appa(t1.full(), t2.full());

                if matches!(partial, Self::App(box Self::Abs(..), _)) {
                    partial.full()
                } else {
                    partial
                }
            }
            Self::Abs(t1) => absa(t1.full()),
            Self::If(cond, consq, alt) => {
                let eval_cond = cond.full();
                match eval_cond {
                    Self::True => consq.full(),
                    Self::False => alt.full(),
                    _ => self.clone(),
                }
            }
            Self::Pred(box t1) => match t1 {
                Self::Zero => Self::Zero,
                Self::Succ(t2) => t2.full(),
                _ => t1.full(),
            },
            Self::IsZero(t1) => {
                let t1_eval = t1.full();
                match t1_eval {
                    TermAnon::Zero => TermAnon::True,
                    TermAnon::Succ(_) => TermAnon::False,
                    _ => Self::IsZero(box t1_eval),
                }
            }
            Self::Cons(t1, t2) => Self::Cons(box t1.full(), box t2.full()),
            Self::IsNil(t1) => {
                let t1_eval = t1.full();
                match t1_eval {
                    TermAnon::Nil => TermAnon::True,
                    TermAnon::Cons(..) => TermAnon::False,
                    _ => TermAnon::IsNil(box t1_eval),
                }
            }
            Self::Head(box Self::Cons(box head, _)) => head.full(),
            Self::Tail(box Self::Cons(_, box tail)) => tail.full(),
            _ => self.clone(),
        }
    }
}

impl Term {
    pub fn dtype(&self) -> Result<Type, &'static str> {
        self.dtype_priv(&HashMap::new())
    }

    fn dtype_priv(&self, ctx: &HashMap<String, Type>) -> Result<Type, &'static str> {
        match self {
            Term::True | Term::False => Ok(Type::Bool),
            Term::If(cond, consq, alt) => {
                let cond_type = cond.dtype_priv(ctx)?;
                let consq_type = consq.dtype_priv(ctx)?;
                let alt_type = alt.dtype_priv(ctx)?;
                if cond_type != Type::Bool {
                    Err("non-boolean condition")
                } else if consq_type != alt_type {
                    Err("diverging conditional types")
                } else {
                    Ok(alt_type)
                }
            }
            Term::Zero => Ok(Type::Nat),
            Term::Succ(t1) | Term::Pred(t1) => {
                if t1.dtype_priv(ctx)? == Type::Nat {
                    Ok(Type::Nat)
                } else {
                    Err("non-nat pred/succ")
                }
            }
            Term::IsZero(t1) => {
                let t1_type = t1.dtype_priv(ctx)?;
                if t1_type == Type::Nat {
                    Ok(Type::Bool)
                } else {
                    Err("non-nat iszero")
                }
            }
            Term::Var(binding) => match ctx.get(binding) {
                Some(t) => Ok(t.clone()),
                None => Err("untyped variable"),
            },
            Term::App(t1, t2) => {
                let t1_type = t1.dtype_priv(ctx)?;
                let t2_type = t2.dtype_priv(ctx)?;
                match t1_type {
                    Type::Con(box a, box b) if a == t2_type => Ok(b),
                    _ => Err("invalid application type"),
                }
            }
            Term::Abs(binding, dtype, body) => {
                let mut ctx = ctx.clone();
                ctx.insert(binding.into(), dtype.clone());
                let body_type = body.dtype_priv(&ctx)?;
                Ok(Type::Con(box dtype.clone(), box body_type))
            }
            Term::Fix(t1) => {
                let t1_type = t1.dtype()?;
                match t1_type {
                    Type::Con(_, box b) => Ok(b),
                    _ => Err("non-arrow fix"),
                }
            }
            Term::Nil(dtype) => Ok(Type::List(box dtype.clone())),
            Term::Cons(t1, t2) => {
                let t1_type = t1.dtype_priv(&ctx)?;
                let t2_type = t2.dtype_priv(&ctx)?;
                if t2_type == Type::List(box t1_type.clone()) {
                    Ok(t2_type)
                } else {
                    Err("inconsistent list typing")
                }
            }
            Term::IsNil(t1) => {
                t1.dtype_priv(&ctx)?;
                Ok(Type::Bool)
            }
            Term::Head(t1) => match t1.dtype_priv(&ctx)? {
                Type::List(list_contents) => Ok(*list_contents),
                _ => Err("non-list head"),
            },
            Term::Tail(t1) => match t1.dtype_priv(&ctx)? {
                t1_type @ Type::List(_) => Ok(t1_type),
                _ => Err("non-list tail"),
            },
        }
    }

    pub fn remove_names(&self, ctx: &Vec<String>) -> TermAnon {
        match self {
            Term::Var(x) => {
                let anon = ctx.iter().rev().position(|c| c == x).unwrap();
                TermAnon::Var(anon)
            }
            Term::Abs(x, _, t1) => {
                let mut ctx = ctx.clone();
                ctx.push(x.into());
                TermAnon::Abs(box t1.remove_names(&ctx))
            }
            Term::App(box t1, box t2) => {
                TermAnon::App(box t1.remove_names(ctx), box t2.remove_names(ctx))
            }
            Term::Succ(t1) => TermAnon::Succ(box t1.remove_names(ctx)),
            Term::Pred(t1) => TermAnon::Pred(box t1.remove_names(ctx)),
            Term::IsZero(t1) => TermAnon::IsZero(box t1.remove_names(ctx)),
            Term::Fix(t1) => TermAnon::Fix(box t1.remove_names(ctx)),
            Term::If(cond, consq, alt) => TermAnon::If(
                box cond.remove_names(ctx),
                box consq.remove_names(ctx),
                box alt.remove_names(ctx),
            ),
            Term::True => TermAnon::True,
            Term::False => TermAnon::False,
            Term::Zero => TermAnon::Zero,
            Term::Nil(_) => TermAnon::Nil,
            Term::Cons(t1, t2) => {
                TermAnon::Cons(box t1.remove_names(ctx), box t2.remove_names(ctx))
            }
            Term::IsNil(t1) => TermAnon::IsNil(box t1.remove_names(ctx)),
            Term::Head(t1) => TermAnon::Head(box t1.remove_names(ctx)),
            Term::Tail(t1) => TermAnon::Tail(box t1.remove_names(ctx)),
        }
    }

    pub fn to_anon_and_ctx(&self) -> (TermAnon, Vec<String>) {
        let mut h = HashSet::new();
        self.extract_vars(&mut h);
        let context: Vec<String> = h.into_iter().collect();
        (self.remove_names(&context), context)
    }

    pub fn to_anon(&self) -> TermAnon {
        let (anon, _) = self.to_anon_and_ctx();
        anon
    }

    fn extract_vars(&self, h: &mut HashSet<String>) {
        match self {
            Term::Var(name) => {
                h.insert(name.into());
            }
            Term::If(cond, consq, alt) => {
                cond.extract_vars(h);
                consq.extract_vars(h);
                alt.extract_vars(h);
            }
            Term::App(t1, t2) | Term::Cons(t1, t2) => {
                t1.extract_vars(h);
                t2.extract_vars(h);
            }
            Term::Abs(_, _, t1)
            | Term::Succ(t1)
            | Term::IsZero(t1)
            | Term::Fix(t1)
            | Term::Pred(t1)
            | Term::IsNil(t1)
            | Term::Head(t1)
            | Term::Tail(t1) => t1.extract_vars(h),
            Term::False | Term::True | Term::Zero | Term::Nil(_) => (),
        }
    }

    pub fn full(&self) -> TermAnon {
        self.to_anon().full()
    }
}
