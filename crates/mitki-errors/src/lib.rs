use std::fmt::Display;

pub use annotate_snippets::Renderer;
use annotate_snippets::{Level, Snippet};
pub use text_size::TextRange;

#[salsa::accumulator]
pub struct Diagnostic {
    pub message: String,
    pub range: TextRange,
}

impl Diagnostic {
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
