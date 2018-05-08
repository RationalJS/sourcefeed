open BsOspec.Esm;
open Routes;
open Routes.Router;
open Routes.Middleware;

open Test;

describe("Basic route matching", () => {

  test("single route", () => {
    let router = route &&& get("/hello") &&& literal("hi");

    makeReq("GET", "/hello")
    |> router
    |> getRes( expectJson("hi") );
  });

  test("multiple routes", () => {
    let router = route
      &&& get("/one") &&& literal(10)
      ||| get("/two") &&& literal(20);

    makeReq("GET", "/one")
    |> router
    |> getRes( expectJson(10) );

    makeReq("GET", "/two")
    |> router
    |> getRes( expectJson(20) );
  });

  testAsync("async route", done_ => {
    let slowLiteral = (x) => (r) =>
      async @@ delay(50, () => r |> literal(x));

    let router = route &&& get("/hello") &&& slowLiteral("hi");

    makeReq("GET", "/hello")
    |> router
    |> getRes( expectJson("hi") >>% done_ );
  });

  testAsync("double async route", done_ => {
    let pause = (r) => {
      delay(50, () => next(r)) |> async;
    };
    let slowLiteral = (x) => (r) =>
      delay(50, () => r |> literal(x)) |> async;

    let router = route &&& get("/goodbye") &&& pause &&& slowLiteral("bye");

    makeReq("GET", "/goodbye")
    |> router
    |> getRes( expectJson("bye") >>% done_ );
  });

  test("middleware adding to context", () => {
    let yield_x = (r) => next_assign(r, { "x": 10 });

    let router = route
      &&& get("/")
      &&& yield_x
      &&& (r => r |> sendJson'(200,r.ctx##x));

    makeReq("GET", "/")
    |> router
    |> getRes( expectJson(10) );
  });

})
