use std::fmt::Display;

pub use annotate_snippets::Renderer;
use annotate_snippets::{Level, Snippet};
pub use text_size::TextRange;

#[salsa::accumulator]
pub struct Diagnostic {
    message: String,
    range: TextRange,
}

impl Diagnostic {
    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn error(message: impl Into<String>, range: TextRange) -> Self {
        Self { message: message.into(), range }
    }

    pub fn render<'a>(
        &'a self,
        renderer: &'a Renderer,
        path: &'a str,
        text: &'a str,
    ) -> impl Display + 'a {
        let message = Level::Error.title(&self.message).snippet(
            Snippet::source(text)
                .origin(path)
                .annotation(Level::Error.span(self.range.into()).label("here"))
                .fold(true),
        );
        renderer.render(message)
    }
}
