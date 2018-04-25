open Routes;
open Routes.Router;
open Routes.Middleware;

let routes =
route
&&& get("/")
&&& literal("hello")

||| route
&&& get("/about") &&& literal({ "name": "alice" })

||| route
&&& get("/me")
&&& AeroCookie.m
&&& Api.auth
&&& (r => {
  Js.log("Found user: " ++ r.ctx##user);
  next(r)
})
&&& (r => r
  |> status(200)
  |> send_json({ "username": r.ctx##user })
);

let port = 3737;
let hostname = "127.0.0.1";
let server = HttpServer.create(routes);

server##listen(port, hostname, () => {
  Js.log("Listening on port " ++ string_of_int(port))
});
