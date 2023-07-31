use std::collections::HashSet;
use std::fmt::Display;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Var(String),
    Abs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TermAnon {
    Var(usize),
    Abs(Box<TermAnon>),
    App(Box<TermAnon>, Box<TermAnon>),
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
        if let Ok(i) = self.to_nat() {
            write!(f, "{i}")
        } else {
            match self {
                _ if self == &Term::succ().to_anon() => write!(f, "succ"),
                _ if self == &Term::tru().to_anon() => write!(f, "tru"),
                _ if self == &Term::zero().to_anon() => write!(f, "zero"),
                _ if self == &Term::test().to_anon() => write!(f, "test"),
                _ if self == &Term::and().to_anon() => write!(f, "and"),
                _ if self == &Term::not().to_anon() => write!(f, "not"),
                _ if self == &Term::or().to_anon() => write!(f, "or"),
                _ if self == &Term::plus().to_anon() => write!(f, "plus"),
                TermAnon::Var(name) => write!(f, "{}", name),
                TermAnon::App(left, right) => write!(f, "({} {})", left, right),
                TermAnon::Abs(body) => write!(f, "(λ. {})", body),
            }
        }
    }
}

// TODO nice printing, especially for adjacent names
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(i) = self.to_anon().to_nat() {
            write!(f, "{i}")
        } else {
            match self {
                _ if self.to_anon() == Term::succ().to_anon() => write!(f, "succ"),
                _ if self.to_anon() == Term::tru().to_anon() => write!(f, "tru"),
                _ if self.to_anon() == Term::zero().to_anon() => write!(f, "zero"),
                _ if self.to_anon() == Term::test().to_anon() => write!(f, "test"),
                _ if self.to_anon() == Term::and().to_anon() => write!(f, "and"),
                _ if self.to_anon() == Term::not().to_anon() => write!(f, "not"),
                _ if self.to_anon() == Term::or().to_anon() => write!(f, "or"),
                _ if self.to_anon() == Term::plus().to_anon() => write!(f, "plus"),
                Term::Var(name) => write!(f, "{}", name),
                Term::App(left, right) => write!(f, "({} {})", left, right),
                Term::Abs(name, body) => write!(f, "(λ{} . {})", name, body),
            }
        }
    }
}

pub fn var(name: &str) -> Term {
    Term::Var(name.to_string())
}

pub fn abs(name: &str, body: Term) -> Term {
    Term::Abs(name.to_string(), box body)
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
            TermAnon::App(t1, t2) => {
                TermAnon::App(box t1.shift_cutoff(c, d), box t2.shift_cutoff(c, d))
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
            Self::App(t1, t2) => appa(t1.sub(j, s), t2.sub(j, s)),
        }
    }

    pub fn full(&self) -> Self {
        match self {
            Self::App(box Self::Abs(t12), box t2) => t12.sub(0, &t2.shift(1)).shift(-1).full(),
            Self::App(t1, t2) => {
                let partial = appa(t1.full(), t2.full());

                if matches!(partial, Self::App(box Self::Abs(..), _)) {
                    partial.full()
                } else {
                    partial
                }
            }
            Self::Abs(t1) => absa(t1.full()),
            _ => self.clone(),
        }
    }

    pub fn to_nat(&self) -> Result<usize, &'static str> {
        match self {
            Self::Abs(box Self::Abs(body)) => Ok(body.to_nat_priv(0)?),
            _ => Err("nan"),
        }
    }

    fn to_nat_priv(&self, count: usize) -> Result<usize, &'static str> {
        match self {
            Self::Var(name) if name == &0 => Ok(count),
            Self::App(box t1, t2) if t1 == &vara(1) => t2.to_nat_priv(count + 1),
            _ => Err("nan"),
        }
    }
}

impl Term {
    pub fn remove_names(&self, ctx: &Vec<String>) -> TermAnon {
        match self {
            Term::Var(x) => {
                let anon = ctx.iter().rev().position(|c| c == x).unwrap();
                TermAnon::Var(anon)
            }
            Term::Abs(x, t1) => {
                let mut ctx = ctx.clone();
                ctx.push(x.into());
                TermAnon::Abs(box t1.remove_names(&ctx))
            }
            Term::App(box t1, box t2) => {
                TermAnon::App(box t1.remove_names(ctx), box t2.remove_names(ctx))
            }
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
            Term::Abs(_, body) => body.extract_vars(h),
            Term::App(t1, t2) => {
                t1.extract_vars(h);
                t2.extract_vars(h);
            }
        }
    }

    pub fn full(&self) -> TermAnon {
        self.to_anon().full()
    }
}
