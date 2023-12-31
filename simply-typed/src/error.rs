use std::error::Error;
use std::fmt::Display;
use std::path::PathBuf;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

#[derive(Debug, Clone)]
pub struct LcError {
    pub label: String,
    range: (usize, usize),
}

impl LcError {
    pub fn new<S>(label: &S, range: (usize, usize)) -> Self
    where
        S: Into<String> + std::fmt::Display,
    {
        Self {
            label: label.to_string(),
            range,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LcErrorReporter {
    error: LcError,
    path: PathBuf,
    source: String,
}

impl LcErrorReporter {
    pub fn new(error: LcError, path: PathBuf, source: String) -> Self {
        Self {
            error,
            path,
            source,
        }
    }
}

impl Error for LcErrorReporter {}

impl Display for LcErrorReporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let snip = Snippet {
            title: Some(Annotation {
                label: Some(&self.error.label),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            footer: vec![],
            slices: vec![Slice {
                source: &self.source,
                line_start: 0,
                origin: self.path.to_str(),
                fold: false,
                annotations: vec![SourceAnnotation {
                    label: &self.error.label,
                    range: self.error.range,
                    annotation_type: AnnotationType::Error,
                }],
            }],
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };
        let dl = DisplayList::from(snip);
        write!(f, "{dl}")
    }
}
