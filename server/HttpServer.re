
module RawHttp = {
  type req = Js.t({.
    headers: Js.Dict.t(string),
    url: string,
    _method: string,
  });

  type res = Js.t({.
    headers: Js.Dict.t(string),
    statusCode: int,
    [@bs.meth]
    writeHead: (int, Js.Dict.t(string)) => unit,
    [@bs.meth]
    _end: (string) => unit
  });

  type server = Js.t({.
    [@bs.meth]
    listen : (int, string, (. unit => unit)) => unit
  });

  type http = Js.t({.
    [@bs.meth]
    createServer : ((. req,res) => unit) => server
  });

  [@bs.module]
  external http : http = ""
};

let print_handler_action = (a) => Routes.(switch(a) {
  | Pass(_) => "Pass"
  | Fail => "Fail"
  | Halt(_) => "Halt"
  | Async(_) => "Async"
});

let makeReq = (raw_req : RawHttp.req) => {
  Routes.headers: raw_req##headers,
  url: raw_req##url,
  method: raw_req##_method,
};

let makeRes = () => Routes.ResFresh(Routes.emptyHeaders());

let makeRouteContext = (raw_req) => {
  Routes.req: makeReq(raw_req),
  res: makeRes(),
  ctx: (Js.Obj.empty() : Routes.fresh_ctx),
  matched: "",
};

let create = (routes) =>
  RawHttp.http##createServer( (. req, res) => {
    Js.log(req##_method ++ " " ++ req##url);
    let plan : Routes.handler_action(Routes.endpoint, 'inital_ctx, Routes.fresh, Routes.complete)
             = routes(makeRouteContext(req));
    let rec execute = Routes.((p) =>
      switch (p) {
        | Halt({ res: ResEnded(status_code, headers, body) }) =>
          res##writeHead(status_code, headers);
          switch(body) {
            | Some(content) => res##_end(content)
            | None => res##_end("")
          }
        | Fail =>
          res##writeHead(404, emptyHeaders());
          res##_end("No such route: " ++ req##_method ++ " " ++ req##url)
        | Async(task) =>
          task |> Task.run(execute)
        | other =>
          res##writeHead(500, emptyHeaders());
          Js.log("Invalid response: " ++ print_handler_action(other));
          res##_end("Server error")
      }
    );
    execute(plan)
  });
