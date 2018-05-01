open Routes;
open BsOspec;

exception AssertionError(string);

let stringify = (value) => Js.Json.stringifyAny(value) |> Belt.Option.getExn;

let makeReq = (~headers=Js.Dict.empty(), _method, url) =>
  HttpServer.makeRouteContext({
    "headers": headers,
    "url": url,
    "_method": _method,
  });

let rec getRes = (callback, p) =>
  switch (p) {
    | Routes.Async(task) =>
      task |> Task.run(getRes(callback)) |> ignore
    | other => callback(other)
  }
;

let expectJson = (expected, actual) => switch(actual) {
  | Halt({ res: ResEnded(_, _, body) }) => switch(body) {
      | Some(body) =>
        body |> deepEquals(expected |> stringify)
      | None =>
        raise(AssertionError("Response did not send a body"))
    }
  | _ =>
    raise(AssertionError("Response did not end"))
};

type timeoutId;
[@bs.val] [@bs.val] external setTimeout : ([@bs.uncurry] (unit => unit), int) => timeoutId = "";
let delay = (ms, f) => Task.make(resolve =>
  setTimeout(() => f() |> resolve, ms) |> ignore
);

/* Convenience helper mostly for calling finished() in async tests */
let (>>%) = (f,g,x) => { f(x); g() };
