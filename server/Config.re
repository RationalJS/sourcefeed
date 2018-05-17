
type t = {.
  "argv" : array(string),
  "arch" : string,
  "env" : Js.Dict.t(string),
  [@bs.meth] "abort" : unit => unit,
  [@bs.meth] "chdir" : string => unit,
  [@bs.meth] "cwd" : unit => string,
  [@bs.meth] "disconnect" : unit => unit,
};

[@bs.module]
external process : t = "";

let env = switch (process##env |> Js.Dict.get(_, "NODE_ENV")) {
  | Some(env) => env
  | None => "development" /* Default */
};

let dev = env == "development";
let test = env == "test";
let prod = env == "production";
