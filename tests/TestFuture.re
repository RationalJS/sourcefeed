open BsOspec;

type timeoutId;
[@bs.val] [@bs.val] external setTimeout : ([@bs.uncurry] (unit => unit), int) => timeoutId = "";

describe("Future", () => {

  let delay = (ms, f) => Future.make(resolve =>
    setTimeout(() => f() |> resolve, ms) |> ignore
  );

  test("sync chaining", () => {
    Future.value("one")
    |> Future.map(s => s ++ "!")
    |> Future.map(s => {
      s |> equals("one!");
    });
  });

  testAsync("async chaining", done_ => {
    delay(25, () => 20)
    |> Future.map(s => string_of_int(s))
    |> Future.map(s => s ++ "!")
    |> Future.get(s => {
      s |> equals("20!");
      done_();
    });
  });

  test("tap", () => {
    let v = ref(0);

    Future.value(99)
    |> Future.tap(n => v := n+1)
    |> Future.map(n => n - 9)
    |> Future.get(n => {
      n |> equals(90);
      v^ |> equals(100);
    });
  });

  test("multiple runs", () => {
    let count = ref(0);
    let future = Future.make(resolve => {
      count := count^ + 1;
      resolve(count^);
    });
    count^ |> equals(1);

    future |> Future.get(c => {
      c |> equals(1);
    });
    count^ |> equals(1);

    future |> Future.get(c => {
      c |> equals(1);
    });
    count^ |> equals(1);
  });

  testAsyncOnly("multiple async runs", done_ => {
    let count = ref(0);
    let future = delay(25, () => 0)
    |> Future.map(_ => {
      count := count^ + 1;
    });

    count^ |> equals(0, ~m="Count is async");

    future |> Future.get(_ => {
      count^ |> equals(1, ~m="Runs after previous future");
    });
    count^ |> equals(0, ~m="Count is async (2)");

    future |> Future.get(_ => {
      count^ |> equals(1, ~m="Previous future only runs once");
    });
    count^ |> equals(0, ~m="Count is async (3)");

    future |> Future.get(_ => done_());
  });

});
