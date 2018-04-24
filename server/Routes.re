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
type headers_sent;
type complete;

type res_body = option(string);
type res(_) =
  | ResFresh(headers) : res(fresh)
  | ResHeadersSent(int, headers) : res(headers_sent)
  | ResEnded(int, headers, res_body) : res(complete);

type route_context('a, 'res_status) = {
  req: req,
  res: res('res_status),
  ctx: 'a,
  matched: string, /* The amount of the url matched so far */
};

/* TODO: Add feature to send headers early */
type handler_action('same, 'change, 's1, 's2) =
  | Pass(route_context('change, 's2))
  | Halt(route_context('same, complete))
  | Fail
  | Async(Task.t(handler_action('same, 'change, 's1, 's2)))
;

type handler('a,'b,'s1,'s2) = route_context('a,'s1) => handler_action('a,'b,'s1,'s2);

let pass = (r) => Pass(r);
let async = (task) => Async(task);

let status = (code, r) => {
  let ResFresh(headers) = r.res;
  {
    ...r,
    res: ResHeadersSent(code, headers)
  }
};


let chain = (e1, e2) => (r1) =>
  switch(e1(r1)) {
    | Pass(r2) => e2(r2)
    | Halt(r2) => Halt(r2)
    | Fail => Fail
    | Async(task) =>
      let rec handle = (action) => switch(action) {
        | Pass(r2) => e2(r2)
        | Halt(r2) => Halt(r2)
        | Fail => Fail
        /*
         * Recursively "unwrap" async task results.
         * This is here so OCaml types the function correctly,
         * but it also allows middleware to return other middleware!
         */
        | Async(task2) => Async(task2 |> Task.map(handle))
      };
      task
      |> Task.map(handle)
      |> (task => Async(task))
  };
let (&&&) = chain;

let find = (e1, e2) => (r1) =>
  switch(e1(r1)){
    | Pass(r2) => Pass(r2)
    | Halt(r2) => Halt(r2)
    | Fail => e2(r1)
    | Async(task) =>
      let rec handle = (action) => switch(action) {
        | Pass(r2) => Pass(r2)
        | Halt(r2) => Halt(r2)
        | Fail => e2(r1)
        /*
         * Recursively "unwrap" async task results.
         * This is here so OCaml types the function correctly,
         * but it also allows middleware to return other middleware!
         */
        | Async(task2) => Async(task2 |> Task.map(handle))
      };
      task
      |> Task.map(handle)
      |> (task => Async(task))
  };
let (|||) = find;


let get = (path) => (r : route_context('a, fresh)) =>
  r.req.method == "GET" && r.req.url == path
  ? Pass({ ...r, matched: path })
  : Fail;

let send_json = (content, r) => {
  let ResHeadersSent(code, headers) = r.res;
  Halt({
    ...r,
    ctx: endpoint,
    res: ResEnded(code, headers, content |> Js.Json.stringifyAny)
  })
};

let literal = (content) => (r) =>
  Halt({
    ...r,
    ctx: endpoint,
    res: ResEnded(200, emptyHeaders(), content |> Js.Json.stringifyAny)
  });
