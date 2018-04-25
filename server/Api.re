open Routes;
open Routes.Middleware;
module Dict = Js.Dict;
module Re = Js.Re;
module Option = Belt.Option;


let bearerRe = [%re "/^Bearer (.*)$/"];
let parseBearerToken = (str) => {
  let result = str
    |> Re.exec(_, bearerRe)
    |> Option.map(_, Re.captures);
  switch(result) {
    | Some([| _, token |]) => Js.Nullable.toOption(token)
    | Some(x) => Js.log("wat"); Js.log(x); None
    | _ => None
  }
};

let auth = (r) => {
  let result = r.ctx##cookies
    |> Dict.get(_, "Authentication")
    |> Option.flatMap(_, parseBearerToken);

  switch(result) {
    | Some(token) =>
      next_assign(r, { "user": token })
    | None =>
      r
      |> status(401)
      |> send_json({ "reason": "not_signed_in" })
  }
};
