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
type res_type =
  | Ok_200(option(string))
  | Redirect_302(string)
  | Error_500(string);

type res = Res(headers, res_type);

type route_context('a) = {
  req: req,
  res: res,
  ctx: 'a,
  matched: string, /* The amount of the url matched so far */
};

[@bs.deriving accessors]
type handler_action('same, 'change) =
  | Pass(route_context('change))
  | Halt(route_context('same))
  | Fail
  | Async(Task.t(handler_action('same, 'change)))
;

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


let get = (path) => (route) =>
  route.req.method == "GET" && route.req.url == path
  ? Pass({ ...route, matched: path })
  : Fail;

let send_json = (r, code, content) => switch(code, r.res) {
  | (200, Res(headers, _)) =>
    Halt({
      ...r,
      ctx: endpoint,
      res: Res(headers, Ok_200(content |> Js.Json.stringifyAny))
    })
  | _ => raise(ServerError("Invalid status code: " ++ string_of_int(code)))
};

let literal = (content) => (r) =>
  Halt({
    ...r,
    ctx: endpoint,
    res: Res(emptyHeaders(), Ok_200(content |> Js.Json.stringifyAny))
  });
