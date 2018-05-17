open BsNode;
open BsOspec.Esm;
open Routes;
open Routes.Router;
open Routes.Middleware;

open Test;

describe("AeroLess", () => {

  testAsync("file", done_ => {
    let router = route
      &&& get("/assets/style.css")
      &&& AeroLess.file("middleware/fixtures/less/index.less" |> getTestPath);

    makeReq("GET", "/assets/style.css")
    |> router
    |> getRes(
      expectBody(~headers={ "content-type": "text/css" }, "body { color: red; }\n\n")
      >>% done_
    );
  });

});
