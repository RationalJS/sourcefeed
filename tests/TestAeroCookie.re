open BsOspec;
open Routes;
open Routes.Router;
open Routes.Middleware;

open Test;

describe("AeroCookie", () => {

  test("middleware", () => {
    let router = route
      &&& get("/hello")
      &&& AeroCookie.m
      &&& (r => r |>
        send_json'(200, { "c": r.ctx##cookies |> Js.Dict.unsafeGet(_, "my-cookie") })
      );

    makeReq(
      ~headers=Js.Dict.fromList([("cookie", "my-cookie=yum!")]),
      "GET", "/hello")
    |> router
    |> getRes( expectJson({ "c": "yum!" }) );
  });

});
