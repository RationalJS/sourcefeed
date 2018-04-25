open BsOspec;
open Routes;
open Routes.Router;
open Routes.Middleware;

open Test;

describe("Basic route matching", () => {

  test("single route", () => {
    let router = route &&& get("/hello") &&& literal("hi");

    makeReq("GET", "/hello")
    |> router
    |> execute( expectJson("hi") );
  });

  test("multiple routes", () => {
    let router = route
      &&& get("/one") &&& literal(10)
      ||| get("/two") &&& literal(20);

    makeReq("GET", "/one")
    |> router
    |> execute( expectJson(10) );

    makeReq("GET", "/two")
    |> router
    |> execute( expectJson(20) );
  });

  testAsync("async route", finished => {
    let slowLiteral = (x) => (r) =>
      async @@ delay(50, () => r |> literal(x));

    let router = route &&& get("/hello") &&& slowLiteral("hi");

    makeReq("GET", "/hello")
    |> router
    |> execute( expectJson("hi") >>% finished );
  });

  testAsync("double async route", finished => {
    let pause = (r) => {
      delay(50, () => next(r)) |> async;
    };
    let slowLiteral = (x) => (r) =>
      delay(50, () => r |> literal(x)) |> async;

    let router = route &&& get("/goodbye") &&& pause &&& slowLiteral("bye");

    makeReq("GET", "/goodbye")
    |> router
    |> execute( expectJson("bye") >>% finished );
  });

  test("middleware adding to context", () => {
    let yield_x = (r) => next_assign(r, { "x": 10 });

    let router = route
      &&& get("/")
      &&& yield_x
      &&& (r => r |> send_json'(200,r.ctx##x));

    makeReq("GET", "/")
    |> router
    |> execute( expectJson(10) );
  });

})
