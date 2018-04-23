
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

let print_res = (res) => Routes.(switch(res) {
  | Ok_200(content) => "[200: " ++ Belt.Option.getWithDefault(content, "") ++ "]"
  | Redirect_302(url) => "[302: " ++ url ++ "]"
  | Error_500(err) => "[500: " ++ err ++ "]"
});
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

let makeRes = () => Routes.(Res(emptyHeaders(), Ok_200(None)));

let makeRouteContext = (raw_req, ctx) => {
  Routes.req: makeReq(raw_req),
  res: makeRes(),
  ctx: ctx,
  matched: "",
};

let create = (ctx, routes) =>
  RawHttp.http##createServer( (. req, res) => {
    Js.log(req##_method ++ " " ++ req##url);
    let plan : Routes.handler_action(Routes.endpoint, 'a)
             = routes(makeRouteContext(req, ctx));
    let rec execute = Routes.((p) =>
      switch (p) {
        | Halt({ res: Res(headers,Ok_200(body)) }) =>
          res##writeHead(200, headers);
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
