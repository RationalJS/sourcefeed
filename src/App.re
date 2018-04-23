open Routes;

type signed_out = { signed_out: bool };
type signed_in = { signed_in: bool, user: string };

type timeoutId;
[@bs.val] [@bs.val] external setTimeout : ([@bs.uncurry] (unit => unit), int) => timeoutId = "";
let wait = (ms, f) => Task.make(resolve =>
  setTimeout(() => f() |> resolve, ms) |> ignore
);

let auth = (r) => {
  let _state = r.ctx.signed_out;
  let ctx = { signed_in: true, user: "alice" };
  wait(2000, () => pass({ ...r, ctx })) |> async
};


let routes
= get("/")
  &&& literal("hello")

||| get("/about")
  &&& literal({ "name": "alice" })

||| get("/me")
  &&& auth
  &&& (r => {
    let { user } = r.ctx;
    Js.log("Found user: " ++ user);
    pass(r)
  })
  &&& (r =>
    send_json(r, 200, { "username": r.ctx.user })
  )
;


let port = 3737;
let hostname = "127.0.0.1";
let server = HttpServer.create({ signed_out: true }, routes);

server##listen(port, hostname, () => {
  Js.log("Listening on port " ++ string_of_int(port))
});
