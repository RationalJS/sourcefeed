open BsNode;
open Routes.Router;
open Routes.Middleware;

let getPath = NodePath.join2(NodeGlobal.__dirname);

let routes =
route
&&& prefix("/assets")
&&& AeroStatic.dir("../client/build" |> getPath)

||| route
&&& get("/") &&& AeroStatic.file("../client/index.html" |> getPath);

let port = 3737;
let hostname = "127.0.0.1";

HttpServer.create(routes)
|> HttpServer.listen(port, hostname, () => {
  Js.log("Listening on port " ++ string_of_int(port))
});
