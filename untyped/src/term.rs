use crate::church::*;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(String),
    Abs(String, Box<Term>),
    App(Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TermAnon {
    Var(usize),
    Abs(usize, Box<TermAnon>),
    App(Box<TermAnon>, Box<TermAnon>),
}

impl Display for TermAnon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TermAnon::Var(name) => write!(f, "{}", name),
            TermAnon::App(left, right) => write!(f, "({} {})", left, right),
            TermAnon::Abs(name, body) => write!(f, "(λ{} . {})", name, body),
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

// TODO nice printing, especially for adjacent names
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(i) = self.to_nat() {
            write!(f, "{i}")
        } else {
            match self {
                //            _ if self == &SUCC.clone() => write!(f, "succ"),
                //            _ if self == &TRU.clone() => write!(f, "tru"),
                //            _ if self == &ZERO.clone() => write!(f, "zero"),
                //            _ if self == &TEST.clone() => write!(f, "test"),
                //            _ if self == &AND.clone() => write!(f, "and"),
                //            _ if self == &NOT.clone() => write!(f, "not"),
                //            _ if self == &OR.clone() => write!(f, "or"),
                // TODO just testing this out, isn't quite De Bruijn index
                _ if self.to_anon() == SUCC.to_anon() => write!(f, "succ"),
                _ if self.to_anon() == TRU.to_anon() => write!(f, "tru"),
                _ if self.to_anon() == ZERO.to_anon() => write!(f, "zero"),
                _ if self.to_anon() == TEST.to_anon() => write!(f, "test"),
                _ if self.to_anon() == AND.to_anon() => write!(f, "and"),
                _ if self.to_anon() == NOT.to_anon() => write!(f, "not"),
                _ if self.to_anon() == OR.to_anon() => write!(f, "or"),
                _ if self.to_anon() == PLUS.to_anon() => write!(f, "plus"),
                _ if self.to_anon() == TIMES.to_anon() => write!(f, "times"),
                Term::Var(name) => write!(f, "{}", name),
                Term::App(left, right) => write!(f, "({} {})", left, right),
                Term::Abs(name, body) => write!(f, "(λ{} . {})", name, body),
            }
        }
    }
}

impl Term {
    fn to_nat(&self) -> Result<usize, &'static str> {
        match self {
            Term::Abs(s, box Term::Abs(z, body)) => {
                let count = 0;
                Ok(body.to_nat_priv(s, z, count)?)
            }
            _ => Err("nan"),
        }
    }

    fn to_nat_priv(&self, s: &String, z: &String, count: usize) -> Result<usize, &'static str> {
        match self {
            Term::Var(name) if name == z => Ok(count),
            Term::App(box t1, t2) if t1 == &var(s) => t2.to_nat_priv(s, z, count + 1),
            _ => Err("nan"),
        }
    }

    pub fn to_anon(&self) -> TermAnon {
        let mut h = HashMap::new();
        let mut idx = 0;
        self.to_anon_priv(&mut idx, &mut h)
    }

    fn to_anon_priv(&self, idx: &mut usize, h: &mut HashMap<String, usize>) -> TermAnon {
        match self {
            Term::Var(name) => {
                if let Some(prev_idx) = h.get(name) {
                    TermAnon::Var(*prev_idx)
                } else {
                    *idx += 1;
                    h.insert(name.into(), *idx);
                    TermAnon::Var(*idx)
                }
            }
            Term::App(t1, t2) => {
                let t1_anon = t1.to_anon_priv(idx, h);
                let t2_anon = t2.to_anon_priv(idx, h);
                TermAnon::App(box t1_anon, box t2_anon)
            }
            Term::Abs(name, t1) => {
                let t1_anon = t1.to_anon_priv(idx, h);
                let name_id = if let Some(id) = h.get(name) {
                    *id
                } else {
                    *idx += 1;
                    *idx
                };
                TermAnon::Abs(name_id, box t1_anon)
            }
        }
    }

    pub fn free(&self) -> HashSet<String> {
        let mut h = HashSet::new();
        self.free_extract(&mut h);
        h
    }

    fn free_extract(&self, h: &mut HashSet<String>) {
        match self {
            Term::Var(x) => {
                h.insert(x.to_string());
            }
            Term::Abs(x, t1) => {
                let mut free_t1 = t1.free();
                free_t1.remove(x);
                for name in free_t1 {
                    h.insert(name);
                }
            }
            Term::App(box t1, box t2) => {
                t1.free_extract(h);
                t2.free_extract(h);
            }
        }
    }

    //    fn is_na(&self) -> bool {
    //        matches!(self, Term::Var(_) | Term::App(..))
    //    }
    //
    //    fn is_nanf(&self) -> bool {
    //        match self {
    //            Term::Var(_) => true,
    //            Term::Abs(..) => false,
    //            Term::App(t1, t2) => t1.is_nanf() && t2.is_nf(),
    //        }
    //    }
    //
    //    fn is_nf(&self) -> bool {
    //        match self {
    //            Term::Abs(_, body) => body.is_nf(),
    //            _ => self.is_nanf(),
    //        }
    //    }
    //
    //    // TODO make this work...
    //    pub fn norm(&self) -> Term {
    //        match self {
    //            Term::App(box Term::Abs(x, t12), t2) => t12.sub(x, t2),
    //            Term::App(t1, t2) => {
    //                let t1_norm = if t1.is_na() { t1.norm() } else { *t1.clone() };
    //                let t2_norm = if t2.is_nanf() { t2.norm() } else { *t2.clone() };
    //                let partial = app(t1_norm, t2_norm);
    //
    //                if matches!(partial, Term::App(box Term::Abs(..), _)) {
    //                    partial.norm()
    //                } else {
    //                    partial
    //                }
    //
    //            }
    //            Term::Abs(x, t1) => abs(x, t1.norm()),
    //            Term::Var(_) => self.clone(),
    //        }
    //    }

    pub fn full(&self) -> Term {
        match self {
            Term::App(box Term::Abs(x, t12), box t2) => (t12).sub(x, &t2.full()).full(),
            Term::App(t1, t2) => {
                let partial = app(t1.full(), t2.full());

                if matches!(partial, Term::App(box Term::Abs(..), _)) {
                    partial.full()
                } else {
                    partial
                }
            }
            Term::Abs(x, t1) => abs(x, t1.full()),
            _ => self.clone(),
        }
    }

    fn sub(&self, x: &str, s: &Term) -> Term {
        match self {
            Term::Var(ref y) => {
                if y != x {
                    var(y)
                } else {
                    s.clone()
                }
            }
            Term::Abs(ref y, box t) => {
                if y == x {
                    abs(y, t.clone())
                } else if y != x && !s.free().contains(y) {
                    abs(y, t.sub(x, s))
                } else {
                    panic!(
                        "trying to replace `{}` with `{}` in term `{}`, but it has free variables `{:?}`",
                        x,
                        s,
                        self,
                        s.free()
                    )
                }
            }
            Term::App(t1, t2) => app(t1.sub(x, s), t2.sub(x, s)),
        }
    }
}
