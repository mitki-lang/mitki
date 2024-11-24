use std::fmt::Display;

use annotate_snippets::Snippet;
pub use annotate_snippets::{Level, Renderer};
pub use text_size::TextRange;

#[salsa::accumulator]
pub struct Diagnostic {
    level: Level,
    message: String,
    range: TextRange,
}

impl Diagnostic {
    pub fn new(level: Level, message: impl Into<String>, range: TextRange) -> Self {
        Self { level, message: message.into(), range }
    }

    pub fn error(message: impl Into<String>, range: TextRange) -> Self {
        Self::new(Level::Error, message, range)
    }

    pub fn level(&self) -> Level {
        self.level
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn render<'a>(
        &'a self,
        renderer: &'a Renderer,
        path: &'a str,
        text: &'a str,
    ) -> impl Display + 'a {
        let message = self.level.title(&self.message).snippet(
            Snippet::source(text)
                .origin(path)
                .annotation(Level::Error.span(self.range.into()).label("here"))
                .fold(true),
        );
        renderer.render(message)
    }
}
