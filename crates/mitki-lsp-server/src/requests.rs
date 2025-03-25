use anyhow::Result;

pub(crate) struct RequestDispatcher<'me> {
    request: Option<lsp_server::Request>,
    server: &'me mut crate::Server,
}

impl<'me> RequestDispatcher<'me> {
    pub(crate) fn new(request: lsp_server::Request, server: &'me mut crate::Server) -> Self {
        Self { request: request.into(), server }
    }

    fn parse<R>(&mut self) -> Option<(lsp_server::Request, R::Params)>
    where
        R: lsp_types::request::Request,
    {
        let request = self.request.take_if(|request| request.method == R::METHOD)?;
        match crate::from_json(R::METHOD, &request.params) {
            Ok(params) => Some((request, params)),
            Err(error) => {
                self.server.respond(lsp_server::Response::new_err(
                    request.id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                None
            }
        }
    }

    pub(crate) fn on<R>(mut self, f: fn(&mut crate::Server, R::Params) -> Result<R::Result>) -> Self
    where
        R: lsp_types::request::Request,
    {
        let Some((request, params)) = self.parse::<R>() else {
            return self;
        };

        let result = crate::result_to_response::<R>(request.id, f(self.server, params));
        self.server.respond(result);

        self
    }

    pub(crate) fn finish(self) {
        if let Some(request) = self.request {
            eprintln!("unknown request: {:?}", request);
            self.server.respond(lsp_server::Response::new_err(
                request.id,
                lsp_server::ErrorCode::MethodNotFound as i32,
                "unknown request".to_owned(),
            ));
        }
    }
}
