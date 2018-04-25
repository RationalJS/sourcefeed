open Routes;
open BsOspec;

exception AssertionError(string);

let makeReq = (~headers=Js.Dict.empty(), _method, url) =>
  HttpServer.makeRouteContext({
    "headers": headers,
    "url": url,
    "_method": _method,
  });

let rec execute = Routes.((callback, p) =>
  switch (p) {
    | Async(task) =>
      task |> Task.run(execute(callback))
    | other => callback(other)
  }
);

let expectJson = (expected, actual) => switch(actual) {
  | Halt({ res: ResEnded(_, _, body) }) => switch(body) {
      | Some(body) =>
        body |> deepEquals(expected |> Js.Json.stringifyAny |> Belt.Option.getExn)
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
