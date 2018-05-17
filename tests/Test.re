open Routes;
open BsOspec.Esm;

exception AssertionError(string);

let stringify = (value) => Js.Json.stringifyAny(value) |> Belt.Option.getExn;

let getTestPath = Node.Path.join2(BsNode.NodeGlobal.__dirname);

let makeReq = (~headers=Js.Dict.empty(), method_, url) =>
  HttpServer.makeRouteContextLiteral(method_, url, headers);

let rec getRes = (callback, p) =>
  switch (p) {
    | Routes.Async(future) =>
      future |> Future.get(getRes(callback))
    | other => callback(other)
  }
;

let expectJson = (expected, actual) => switch(actual) {
  | Halt({ res: ResEnded(_,_,_,body) }) => switch(body) {
      | Some(body) =>
        body |> deepEquals(expected |> stringify)
      | None =>
        raise(AssertionError("Response did not send a body"))
    }
  | _ =>
    raise(AssertionError("Response did not end"))
};

let expectBody = (~headers=Js.Obj.empty(), expected, actual) => switch(actual) {
  | Halt({ res: ResEnded(_,_,_, body) } as r) =>

    let checkHeader = (k) =>
      r |> Middleware.getHeader(k) |> Belt.Option.getExn |> equals([%raw "headers[k]"]);

    Js.Obj.keys(headers)
    |> Array.map(k => checkHeader(k));

    switch(body) {
      | Some(body) =>
        body |> equals(expected)
      | None =>
        raise(AssertionError("Response did not send a body"))
    }
  | _ =>
    raise(AssertionError("Response did not end"))
};

let expectHtml = (expected, actual) =>
  try( expectBody(~headers={ "content-type": "text/html" }, expected, actual) ) {
    | AssertionError("Response did not send a body") =>
      raise(AssertionError("Response did not send an html body"))
  };

type timeoutId;
[@bs.val] [@bs.val] external setTimeout : ([@bs.uncurry] (unit => unit), int) => timeoutId = "";
let delay = (ms, f) => Future.make(resolve =>
  setTimeout(() => f() |> resolve, ms) |> ignore
);

/* Convenience helper mostly for calling finished() in async tests */
let (>>%) = (f,g,x) => { f(x); g() };
