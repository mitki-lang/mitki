use anyhow::Result;

use super::{Server, from_json, result_to_response};

pub(crate) struct RequestDispatcher<'me> {
    request: Option<lsp_server::Request>,
    server: &'me mut Server,
}

impl<'me> RequestDispatcher<'me> {
    pub(crate) fn new(request: lsp_server::Request, server: &'me mut Server) -> Self {
        Self { request: request.into(), server }
    }

    fn parse<R>(&mut self) -> Option<(lsp_server::Request, R::Params)>
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned + std::fmt::Debug,
    {
        let request = self.request.take_if(|request| request.method == R::METHOD)?;
        let res = from_json(R::METHOD, &request.params);
        match res {
            Ok(params) => Some((request, params)),
            Err(error) => {
                let response = lsp_server::Response::new_err(
                    request.id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                );
                self.server.respond(response);
                None
            }
        }
    }

    pub(crate) fn on<R>(mut self, f: fn(&mut Server, R::Params) -> Result<R::Result>) -> Self
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned + std::fmt::Debug,
        R::Result: serde::Serialize,
    {
        let (request, params) = match self.parse::<R>() {
            Some(it) => it,
            None => return self,
        };
        let result = f(self.server, params);
        let result = result_to_response::<R>(request.id, result);
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
