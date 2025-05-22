use anyhow::Result;

use super::Server;

pub(crate) struct NotificationDispatcher<'me> {
    notification: Option<lsp_server::Notification>,
    server: &'me mut Server,
}

impl<'me> NotificationDispatcher<'me> {
    pub(crate) fn new(notification: lsp_server::Notification, server: &'me mut Server) -> Self {
        Self { notification: notification.into(), server }
    }

    pub(crate) fn on<N>(&mut self, f: fn(&mut Server, N::Params) -> Result<()>) -> &mut Self
    where
        N: lsp_types::notification::Notification,
    {
        let Some(notification) = self.notification.take() else {
            return self;
        };

        let params = match notification.extract::<N::Params>(N::METHOD) {
            Ok(it) => it,
            Err(lsp_server::ExtractError::JsonError { method, error }) => {
                panic!("Invalid request\nMethod: {method}\n error: {error}",)
            }
            Err(lsp_server::ExtractError::MethodMismatch(notification)) => {
                self.notification = Some(notification);
                return self;
            }
        };

        if let Err(error) = f(self.server, params) {
            eprintln!("{}: {}", N::METHOD, error);
        }

        self
    }

    pub(crate) fn finish(&mut self) {
        if let Some(not) = &self.notification
            && !not.method.starts_with("$/")
        {
            eprintln!("unhandled notification: {not:?}");
        }
    }
}
