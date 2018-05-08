exception ServerError(string);

type headers = Js.Dict.t(string);
let emptyHeaders = () => (Js.Dict.empty() : headers);

type endpoint = { end_: bool }; /* Dummy type to indicate endpoint */
let endpoint = { end_: true }; /* Dummy type to indicate endpoint */

type req = {
  headers: headers,
  method: string,
  url: string
};

type fresh;
type with_status;
type headers_sent;
type ready;
type completed;

type fresh_ctx = Js.t({. });

type res_body = option(string);
type res(_) =
  | ResFresh(headers) : res(fresh)
  | ResWithStatus(int, headers) : res(with_status)
  | ResWithHeadersSent(int, headers) : res(headers_sent)
  | ReqResStream(Js.t(#NodeExtHttp.NodeExtStream.duplexStream)) : res(completed)
  | ResEnded(bool /* headers sent? */, int, headers, res_body) : res(completed);

type route_context('a, 'res_status) = {
  req: req,
  res: res('res_status),
  ctx: 'a,
  urlMatched: string,
};

type handler_action('same, 'change, 's1, 's2) =
  | Pass(route_context('change, 's2))
  | SendHeaders(route_context('change, 's2))
  | Halt(route_context('same, completed))
  | Fail
  | Async(Future.t(handler_action('same, 'change, 's1, 's2)))
;

type handler('a,'b,'s1,'s2) = route_context('a,'s1) => handler_action('a,'b,'s1,'s2);

module Util = {
  let headersSent = (type state, r : route_context('a,state)) => switch(r.res) {
    | ResFresh(_) => false
    | ResWithStatus(_,_) => false
    | ResWithHeadersSent(_,_) => true
    | ResEnded(b,_,_,_) => b
  }
};

module Middleware = {
  let next = (r) => Pass(r);
  let next_assign = (r, obj) => Pass({
    ...r,
    /* Object.assign mutates, so copy before merging. */
    ctx: Js.Obj.(empty() |. assign(r.ctx) |. assign(obj))
  });

  let async = (future) => Async(future);

  let status = (code, r) => {
    let ResFresh(headers) = r.res;
    {
      ...r,
      res: ResWithStatus(code, headers)
    }
  };

  let sendHeadersNow = (r) => {
    let ResWithStatus(code, headers) = r.res;
    SendHeaders({
      ...r,
      res: ResWithHeadersSent(code, headers)
    })
    |> Future.value
    |> async /* Wrap in async so HttpServer.re can handle it */
  };

  let send = (body, r) => {
    let ResWithStatus(code, headers) = r.res;
    Halt({
      ...r,
      ctx: endpoint,
      res: ResEnded(Util.headersSent(r), code, headers, body)
    })
  };

  let sendText = (text, r) => send(Some(text), r);

  let sendJson = (content, r) =>
    r |> send(content |> Js.Json.stringifyAny);

  let sendJson' = (code, content, r) =>
    r |> status(code) |> sendJson(content);

  let sendStream = (stream, r) => {
    let ResFresh(_) = r.res;
    Halt({
      ...r,
      ctx: endpoint,
      res: ReqResStream(stream)
    })
  };
};

module Router = {
  let route = (r : route_context(fresh_ctx,fresh)) => Pass(r);

  let chain = (e1, e2) => (r1) =>
    switch(e1(r1)) {
      | Pass(r2) => e2(r2)
      | SendHeaders(r2) => e2(r2)
      | Halt(r2) => Halt(r2)
      | Fail => Fail
      | Async(future) =>
        let rec handle = (action) => switch(action) {
          | Pass(r2) => e2(r2)
          | SendHeaders(r2) => e2(r2)
          | Halt(r2) => Halt(r2)
          | Fail => Fail
          /*
           * Recursively "unwrap" async future results.
           * This is here so OCaml types the function correctly,
           * but it also allows middleware to directly use other middleware!
           */
          | Async(future2) => Async(future2 |> Future.map(handle))
        };
        future
        |> Future.map(handle)
        |> (future => Async(future))
    };
  let (&&&) = chain;

  let find = (e1, e2) => (r1) =>
    switch(e1(r1)){
      | Pass(r2) => Pass(r2)
      | SendHeaders(r2) => SendHeaders(r2)
      | Halt(r2) => Halt(r2)
      | Fail => e2(r1)
      | Async(future) =>
        let rec handle = (action) => switch(action) {
          | Pass(r2) => Pass(r2)
          | SendHeaders(r2) => SendHeaders(r2)
          | Halt(r2) => Halt(r2)
          | Fail => e2(r1)
          /*
           * Recursively "unwrap" async future results.
           * This is here so OCaml types the function correctly,
           * but it also allows middleware to directly use other middleware!
           */
          | Async(future2) => Async(future2 |> Future.map(handle))
        };
        future
        |> Future.map(handle)
        |> (future => Async(future))
    };
  let (|||) = find;


  let get = (path) => (r : route_context('a, fresh)) =>
    r.req.method == "GET" && r.req.url == path
    ? Pass({ ...r, urlMatched: path })
    : Fail;

  let prefix = (path) => (r : route_context('a, fresh)) =>
    Js.String.startsWith(r.urlMatched ++ path, r.req.url)
    ? Pass({ ...r, urlMatched: r.urlMatched ++ path })
    : Fail;

  let literal = (content) => Middleware.sendJson'(200, content);
};
