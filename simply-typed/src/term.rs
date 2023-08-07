use crate::error::LcError;
use crate::types::Type;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::str::FromStr;

type Loc = (usize, usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    // base lambda calculus terms
    Var(String, Loc),
    Abs(String, Type, Box<Term>, Loc),
    App(Box<Term>, Box<Term>, Loc),
    // Bool
    True,
    False,
    If(Box<Term>, Box<Term>, Box<Term>, Loc),
    // Nat
    Zero,
    Succ(Box<Term>, Loc),
    Pred(Box<Term>, Loc),
    IsZero(Box<Term>, Loc),
    // fixed point
    Fix(Box<Term>, Loc),
    // let binding
    Let(String, Box<Term>, Box<Term>, Loc),
    // lists
    Nil(Type, Loc),
    Cons(Box<Term>, Box<Term>, Loc),
    IsNil(Box<Term>, Loc),
    Head(Box<Term>, Loc),
    Tail(Box<Term>, Loc),
    // variants
    Tag(String, Box<Term>, Type, Loc),
    Case(Box<Term>, Vec<(String, String, Term)>, Loc),
    // records
    Record(Vec<(String, Term)>, Loc),
    Proj(Box<Term>, String, Loc),
    // unit
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TermAnon {
    // base lambda calculus terms
    Var(usize),
    Abs(Box<TermAnon>),
    App(Box<TermAnon>, Box<TermAnon>),
    // Bool
    True,
    False,
    If(Box<TermAnon>, Box<TermAnon>, Box<TermAnon>),
    // Nat
    Zero,
    Succ(Box<TermAnon>),
    Pred(Box<TermAnon>),
    IsZero(Box<TermAnon>),
    // fixed point
    Fix(Box<TermAnon>),
    // let binding
    Let(Box<TermAnon>, Box<TermAnon>),
    // lists
    Nil,
    Cons(Box<TermAnon>, Box<TermAnon>),
    IsNil(Box<TermAnon>),
    Head(Box<TermAnon>),
    Tail(Box<TermAnon>),
    // variants
    Tag(String, Box<TermAnon>),
    Case(Box<TermAnon>, Vec<(String, TermAnon)>),
    // records
    Record(Vec<(String, TermAnon)>),
    Proj(Box<TermAnon>, String),
    // unit
    Unit,
}

impl From<usize> for Term {
    fn from(value: usize) -> Term {
        let mut term = Term::Zero;

        for _ in 0..value {
            term = Term::Succ(box term, (0, 0));
        }

        term
    }
}

impl TryFrom<&Term> for usize {
    type Error = &'static str;

    fn try_from(t: &Term) -> Result<Self, Self::Error> {
        (&t.to_anon()).try_into()
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

        match parser.parse(true) {
            Err(LcError { label, .. }) => Err(label.to_string()),
            Ok(ast) => Ok(ast),
        }
    }
}

impl Display for TermAnon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(name) => write!(f, "{}", name),
            Self::App(left, right) => write!(f, "({} {})", left, right),
            Self::Abs(body) => write!(f, "(λ. {})", body),
            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Zero => write!(f, "ℕ0"),
            Self::Succ(term) => match usize::try_from(self) {
                Ok(i) => write!(f, "ℕ{i}"),
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
            Self::Tag(var_name, t1) => write!(f, "<{var_name}={t1}>"),
            Self::Unit => write!(f, "unit"),
            Self::Case(t1, cases) => {
                write!(f, "\ncase {t1} of\n ")?;

                let len = cases.len();

                for (i, (vname, term)) in cases.iter().enumerate() {
                    write!(f, "  <{vname}> → {term}")?;
                    if i + 1 < len {
                        write!(f, "\n|")?;
                    } else {
                        writeln!(f)?;
                    }
                }

                Ok(())
            }
            Self::Let(t1, t2) => write!(f, "let {t1};\n{t2}"),
            Self::Record(records) => {
                write!(f, "{{")?;
                let len = records.len();
                for (i, (rname, term)) in records.iter().enumerate() {
                    write!(f, "{rname}={term}")?;
                    if i + 1 < len {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            Self::Proj(t1, access) => write!(f, "{t1}.{access}"),
        }
    }
}

// TODO nice printing, especially for adjacent names
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(name, _) => write!(f, "{}", name),
            Self::App(left, right, _) => write!(f, "({} {})", left, right),
            Self::Abs(name, dtype, body, _) => write!(f, "(λ{}:{} . {})", name, dtype, body),
            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Zero => write!(f, "ℕ0"),
            Self::Succ(term, _) => match usize::try_from(self) {
                Ok(i) => write!(f, "ℕ{i}"),
                Err(_) => write!(f, "succ({})", term),
            },
            Self::If(cond, consq, alt, _) => write!(f, "if {cond} then {consq} else {alt}"),
            Self::Pred(term, _) => write!(f, "pred({term})"),
            Self::IsZero(term, _) => write!(f, "iszero({term})"),
            Self::Fix(term, _) => write!(f, "fix({term})"),
            Self::Cons(..) => {
                write!(f, "[")?;
                let mut term = self.clone();
                loop {
                    match term {
                        Term::Cons(t1, box Term::Nil(..), _) => {
                            write!(f, "{}", t1)?;
                            break;
                        }
                        Term::Cons(t1, t2, _) => {
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
            Self::Nil(..) => write!(f, "[]"),
            Self::IsNil(t1, _) => write!(f, "isnil({t1})"),
            Self::Head(t1, _) => write!(f, "head({t1})"),
            Self::Tail(t1, _) => write!(f, "tail({t1})"),
            Self::Tag(var_name, t1, dtype, _) => write!(f, "<{var_name}={t1}> as {dtype}"),
            Self::Unit => write!(f, "unit"),
            Self::Case(t1, cases, _) => {
                write!(f, "\ncase {t1} of\n ")?;

                let len = cases.len();

                for (i, (vname, bind, term)) in cases.iter().enumerate() {
                    write!(f, "  <{vname}={bind}> → {term}")?;
                    if i + 1 < len {
                        write!(f, "\n|")?;
                    } else {
                        writeln!(f)?;
                    }
                }

                Ok(())
            }
            Self::Let(binding, t1, t2, _) => write!(f, "let {binding} = {t1};\n{t2}"),
            Self::Record(records, _) => {
                write!(f, "{{")?;
                let len = records.len();
                for (i, (rname, term)) in records.iter().enumerate() {
                    write!(f, "{rname}={term}")?;
                    if i + 1 < len {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            Self::Proj(t1, access, _) => write!(f, "{t1}.{access}"),
        }
    }
}

pub fn var<T>(name: T, loc: Loc) -> Term
where
    T: Into<String>,
{
    Term::Var(name.into(), loc)
}

pub fn var_no_loc<T>(name: T) -> Term
where
    T: Into<String>,
{
    Term::Var(name.into(), (0, 0))
}

pub fn abs<T>(name: T, t: Type, body: Term, loc: Loc) -> Term
where
    T: Into<String>,
{
    Term::Abs(name.into(), t, box body, loc)
}

pub fn abs_no_loc<T>(name: T, t: Type, body: Term) -> Term
where
    T: Into<String>,
{
    Term::Abs(name.into(), t, box body, (0, 0))
}

pub fn app(l: Term, r: Term, loc: Loc) -> Term {
    Term::App(box l, box r, loc)
}

pub fn app_no_loc(l: Term, r: Term) -> Term {
    Term::App(box l, box r, (0, 0))
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
            TermAnon::False | TermAnon::True | TermAnon::Zero | TermAnon::Nil | TermAnon::Unit => {
                self.clone()
            }
            TermAnon::Cons(t1, t2) => {
                TermAnon::Cons(box t1.shift_cutoff(c, d), box t2.shift_cutoff(c, d))
            }
            TermAnon::IsNil(t1) => TermAnon::IsNil(box t1.shift_cutoff(c, d)),
            TermAnon::Head(t1) => TermAnon::Head(box t1.shift_cutoff(c, d)),
            TermAnon::Tail(t1) => TermAnon::Tail(box t1.shift_cutoff(c, d)),
            TermAnon::Tag(name, t1) => TermAnon::Tag(name.clone(), box t1.shift_cutoff(c, d)),
            TermAnon::Case(t1, case_terms) => {
                let anon_cases: Vec<(String, TermAnon)> = case_terms
                    .iter()
                    .map(|(vname, term)| (vname.clone(), term.shift_cutoff(c + 1, d)))
                    .collect();
                TermAnon::Case(box t1.shift_cutoff(c, d), anon_cases)
            }
            TermAnon::Let(t1, t2) => {
                TermAnon::Let(box t1.shift_cutoff(c, d), box t2.shift_cutoff(c + 1, d))
            }
            TermAnon::Record(records) => TermAnon::Record(
                records
                    .iter()
                    .map(|(rname, term)| (rname.into(), term.shift_cutoff(c, d)))
                    .collect(),
            ),
            TermAnon::Proj(box t1, access) => {
                TermAnon::Proj(box t1.shift_cutoff(c, d), access.into())
            }
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
            TermAnon::False | TermAnon::True | TermAnon::Zero | TermAnon::Nil | TermAnon::Unit => {
                self.clone()
            }
            TermAnon::Cons(t1, t2) => TermAnon::Cons(box t1.sub(j, s), box t2.sub(j, s)),
            TermAnon::IsNil(t1) => TermAnon::IsNil(box t1.sub(j, s)),
            TermAnon::Head(t1) => TermAnon::Head(box t1.sub(j, s)),
            TermAnon::Tail(t1) => TermAnon::Tail(box t1.sub(j, s)),
            TermAnon::Tag(name, t1) => TermAnon::Tag(name.clone(), box t1.sub(j, s)),
            TermAnon::Case(t1, case_terms) => {
                let anon_cases: Vec<(String, TermAnon)> = case_terms
                    .iter()
                    .map(|(vname, term)| (vname.clone(), term.sub(j + 1, &s.shift(1))))
                    .collect();
                TermAnon::Case(box t1.sub(j, s), anon_cases)
            }
            TermAnon::Let(t1, t2) => {
                TermAnon::Let(box t1.sub(j, s), box t2.sub(j + 1, &s.shift(1)))
            }
            TermAnon::Record(records) => TermAnon::Record(
                records
                    .iter()
                    .map(|(rname, term)| (rname.into(), term.sub(j, s)))
                    .collect(),
            ),
            TermAnon::Proj(box t1, access) => TermAnon::Proj(box t1.sub(j, s), access.into()),
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
            Self::Case(case_term, cases) => {
                if let Self::Tag(case_term_var, case_term_untag) = case_term.full() {
                    let (_, select_term) = cases
                        .iter()
                        .filter(|(vname, _)| vname == &case_term_var)
                        .next()
                        .unwrap();
                    select_term.clone().sub(0, &case_term_untag).full()
                } else {
                    // if the case is not a tag (e.g. an abstract var) I choose to just clone for
                    Self::Case(case_term.clone(), cases.to_vec())
                }
            }
            Self::Tag(vname, t1) => Self::Tag(vname.into(), box t1.full()),
            Self::Let(box t1, box t2) => t2.sub(0, &t1.full()).full(),
            Self::Record(records) => Self::Record(
                records
                    .iter()
                    .map(|(rname, term)| (rname.clone(), term.full()))
                    .collect(),
            ),
            Self::Proj(box t1, access) => {
                let t1_eval = t1.full();

                match t1_eval {
                    TermAnon::Record(records) => {
                        let (_, term) = records
                            .iter()
                            .filter(|(rname, _)| rname == access)
                            .next()
                            .unwrap();
                        term.full()
                    }
                    _ => self.clone(),
                }
            }
            _ => self.clone(),
        }
    }
}

impl Term {
    pub fn dtype(&self) -> Result<Type, LcError> {
        self.dtype_priv(&HashMap::new())
    }

    fn dtype_priv(&self, ctx: &HashMap<String, Type>) -> Result<Type, LcError> {
        match self {
            Term::True | Term::False => Ok(Type::Bool),
            Term::If(cond, consq, alt, range) => {
                let cond_type = cond.dtype_priv(ctx)?;
                let consq_type = consq.dtype_priv(ctx)?;
                let alt_type = alt.dtype_priv(ctx)?;
                if cond_type != Type::Bool {
                    Err(LcError::new(&"non-boolean condition", *range))
                } else if consq_type != alt_type {
                    Err(LcError::new(&"diverging conditional types", *range))
                } else {
                    Ok(alt_type)
                }
            }
            Term::Zero => Ok(Type::Nat),
            Term::Succ(t1, range) | Term::Pred(t1, range) => {
                if t1.dtype_priv(ctx)? == Type::Nat {
                    Ok(Type::Nat)
                } else {
                    Err(LcError::new(&"non-nat pred/succ", *range))
                }
            }
            Term::IsZero(t1, range) => {
                let t1_type = t1.dtype_priv(ctx)?;
                if t1_type == Type::Nat {
                    Ok(Type::Bool)
                } else {
                    Err(LcError::new(&"non-nat iszero", *range))
                }
            }
            Term::Var(binding, range) => match ctx.get(binding) {
                Some(t) => Ok(t.clone()),
                None => Err(LcError::new(&"untyped variable", *range)),
            },
            Term::App(t1, t2, range) => {
                let t1_type = t1.dtype_priv(ctx)?;
                let t2_type = t2.dtype_priv(ctx)?;
                match t1_type {
                    Type::Con(box a, box b) if a == t2_type => Ok(b),
                    _ => Err(LcError::new(&"invalid application type", *range)),
                }
            }
            Term::Abs(binding, dtype, body, _) => {
                let mut ctx = ctx.clone();
                ctx.insert(binding.into(), dtype.clone());
                let body_type = body.dtype_priv(&ctx)?;
                Ok(Type::Con(box dtype.clone(), box body_type))
            }
            Term::Fix(t1, range) => {
                let t1_type = t1.dtype()?;
                match t1_type {
                    Type::Con(_, box b) => Ok(b),
                    _ => Err(LcError::new(&"non-arrow fix", *range)),
                }
            }
            Term::Nil(dtype, _) => Ok(Type::List(box dtype.clone())),
            Term::Cons(t1, t2, range) => {
                let t1_type = t1.dtype_priv(&ctx)?;
                let t2_type = t2.dtype_priv(&ctx)?;
                if t2_type == Type::List(box t1_type.clone()) {
                    Ok(t2_type)
                } else {
                    Err(LcError::new(&"inconsistent list typing", *range))
                }
            }
            Term::IsNil(t1, _) => {
                t1.dtype_priv(&ctx)?;
                Ok(Type::Bool)
            }
            Term::Head(t1, range) => match t1.dtype_priv(&ctx)? {
                Type::List(list_contents) => Ok(*list_contents),
                _ => Err(LcError::new(&"non-list head", *range)),
            },
            Term::Tail(t1, range) => match t1.dtype_priv(&ctx)? {
                t1_type @ Type::List(_) => Ok(t1_type),
                _ => Err(LcError::new(&"non-list tail", *range)),
            },
            Term::Tag(var_name, t1, dtype, range) => {
                if let Type::Variant(var_type_vec) = dtype {
                    let mut uniq: HashMap<String, Type> = HashMap::new();
                    // check that type names are unique, and that the tag type is present and
                    // matches in the variant type

                    if !var_type_vec.into_iter().all(|(vname, vtype)| uniq.insert(vname.clone(), vtype.clone()).is_none()  ) {
                    Err(LcError::new(&"variant type with non-unique names", *range))
                    } else if let Some(var_type) = uniq.get(var_name) && var_type == &t1.dtype()? {
                        Ok(dtype.clone())
                    } else {
                    Err(LcError::new(&"tag variant incompatible with variant type", *range))
                    }
                } else {
                    Err(LcError::new(&"tag with non-variant type", *range))
                }
            }
            Term::Unit => Ok(Type::Unit),
            Term::Case(case_term, cases, range) => {
                let case_term_type = case_term.dtype_priv(&ctx)?;

                // all cases should have the same type, which this expression will eval to
                // cases should match and exhaust all possible cases defined in the type
                // for simplicity I require the same order

                if let Type::Variant(case_term_type_vec) = case_term_type {
                    let mut term_types: Vec<Type> = Vec::new();

                    if case_term_type_vec.len() != cases.len() {
                        return Err(LcError::new(&"mismatched case variant", *range));
                    };

                    for ((vname_term, dtype), (vname_case, binding, term)) in
                        std::iter::zip(case_term_type_vec, cases)
                    {
                        let mut ctx = ctx.clone();
                        ctx.insert(binding.into(), dtype.clone());

                        term_types.push(term.dtype_priv(&ctx)?);
                        if &vname_term != vname_case {
                            return Err(LcError::new(&"mismatched case variant", *range));
                        }
                    }

                    let return_type = &term_types[0];
                    if term_types.iter().all(|t| t == return_type) {
                        Ok(return_type.clone())
                    } else {
                        Err(LcError::new(&"inconsistent case types", *range))
                    }
                } else {
                    Err(LcError::new(&"non-tag case", *range))
                }
            }
            Self::Let(binding, t1, t2, _) => {
                let t1_type = t1.dtype_priv(&ctx)?;
                let mut ctx = ctx.clone();
                ctx.insert(binding.into(), t1_type);
                let t2_type = t2.dtype_priv(&ctx)?;
                Ok(t2_type)
            }
            Self::Record(records, _) => {
                let mut record_types: Vec<(String, Type)> = Vec::new();
                for (rname, term) in records {
                    record_types.push((rname.clone(), term.dtype_priv(&ctx)?));
                }
                Ok(Type::Record(record_types))
            }
            Self::Proj(box t1, access, range) => match t1.dtype_priv(&ctx)? {
                Type::Record(records) => {
                    match records.iter().filter(|(rname, _)| rname == access).next() {
                        Some((_, dtype)) => Ok(dtype.clone()),
                        None => Err(LcError::new(&"invalid record accessor", *range)),
                    }
                }
                _ => Err(LcError::new(&"non-record proj", *range)),
            },
        }
    }

    pub fn remove_names(&self, ctx: &Vec<String>) -> TermAnon {
        match self {
            Term::Var(x, _) => {
                let anon = ctx.iter().rev().position(|c| c == x).unwrap();
                TermAnon::Var(anon)
            }
            Term::Abs(x, _, t1, _) => {
                let mut ctx = ctx.clone();
                ctx.push(x.into());
                TermAnon::Abs(box t1.remove_names(&ctx))
            }
            Term::App(box t1, box t2, _) => {
                TermAnon::App(box t1.remove_names(ctx), box t2.remove_names(ctx))
            }
            Term::Succ(t1, _) => TermAnon::Succ(box t1.remove_names(ctx)),
            Term::Pred(t1, _) => TermAnon::Pred(box t1.remove_names(ctx)),
            Term::IsZero(t1, _) => TermAnon::IsZero(box t1.remove_names(ctx)),
            Term::Fix(t1, _) => TermAnon::Fix(box t1.remove_names(ctx)),
            Term::If(cond, consq, alt, _) => TermAnon::If(
                box cond.remove_names(ctx),
                box consq.remove_names(ctx),
                box alt.remove_names(ctx),
            ),
            Term::True => TermAnon::True,
            Term::False => TermAnon::False,
            Term::Zero => TermAnon::Zero,
            Term::Nil(..) => TermAnon::Nil,
            Term::Cons(t1, t2, _) => {
                TermAnon::Cons(box t1.remove_names(ctx), box t2.remove_names(ctx))
            }
            Term::IsNil(t1, _) => TermAnon::IsNil(box t1.remove_names(ctx)),
            Term::Head(t1, _) => TermAnon::Head(box t1.remove_names(ctx)),
            Term::Tail(t1, _) => TermAnon::Tail(box t1.remove_names(ctx)),
            Term::Tag(var_name, t1, _, _) => {
                TermAnon::Tag(var_name.into(), box t1.remove_names(ctx))
            }
            Term::Unit => TermAnon::Unit,
            Term::Case(t1, case_terms, _) => {
                let anon_cases: Vec<(String, TermAnon)> = case_terms
                    .iter()
                    .map(|(varname, bindname, term)| {
                        let mut ctx = ctx.clone();
                        ctx.push(bindname.into());
                        (varname.clone(), term.remove_names(&ctx))
                    })
                    .collect();
                TermAnon::Case(box t1.remove_names(ctx), anon_cases)
            }
            Term::Let(binding, t1, t2, _) => {
                let t1_rem = t1.remove_names(ctx);
                let mut ctx = ctx.clone();
                ctx.push(binding.into());
                let t2_rem = t2.remove_names(&ctx);
                TermAnon::Let(box t1_rem, box t2_rem)
            }
            Term::Record(records, _) => {
                let records_anon: Vec<(String, TermAnon)> = records
                    .iter()
                    .map(|(rname, term)| (rname.clone(), term.remove_names(ctx)))
                    .collect();
                TermAnon::Record(records_anon)
            }
            Term::Proj(t1, access, _) => TermAnon::Proj(box t1.remove_names(ctx), access.into()),
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
            Term::Var(name, _) => {
                h.insert(name.into());
            }
            Term::If(cond, consq, alt, _) => {
                cond.extract_vars(h);
                consq.extract_vars(h);
                alt.extract_vars(h);
            }
            Term::App(t1, t2, _) | Term::Cons(t1, t2, _) | Term::Let(_, t1, t2, _) => {
                t1.extract_vars(h);
                t2.extract_vars(h);
            }
            Term::Abs(_, _, t1, _)
            | Term::Succ(t1, _)
            | Term::IsZero(t1, _)
            | Term::Fix(t1, _)
            | Term::Pred(t1, _)
            | Term::IsNil(t1, _)
            | Term::Head(t1, _)
            | Term::Tail(t1, _)
            | Term::Tag(_, t1, _, _)
            | Term::Proj(t1, _, _) => t1.extract_vars(h),
            Term::Case(t1, cases, _) => {
                t1.extract_vars(h);
                for (_, _, term) in cases {
                    term.extract_vars(h);
                }
            }
            Term::False | Term::True | Term::Zero | Term::Nil(..) | Term::Unit => (),
            Self::Record(records, _) => {
                for (_, term) in records {
                    term.extract_vars(h);
                }
            }
        }
    }

    pub fn full(&self) -> TermAnon {
        let anon = self.to_anon();
        anon.full()
    }
}
