open BsOspec;

type timeoutId;
[@bs.val] [@bs.val] external setTimeout : ([@bs.uncurry] (unit => unit), int) => timeoutId = "";

describe("Task", () => {

  let delay = (ms, f) => Task.make(resolve =>
    setTimeout(() => f() |> resolve, ms) |> ignore
  );

  test("sync chaining", () => {
    Task.value("one")
    |> Task.map(s => s ++ "!")
    |> Task.run(s => {
      s |> equals("one!");
    });
  });

  testAsync("async chaining", done_ => {
    delay(25, () => "two")
    |> Task.map(s => s ++ "!")
    |> Task.run(s => {
      s |> equals("two!");
      done_();
    });
  });

  test("multiple runs", () => {
    let count = ref(0);
    let task = Task.make(resolve => {
      count := count^ + 1;
      resolve(count^);
    });

    task |> Task.run(c => {
      c |> equals(1);
    });
    count^ |> equals(1);

    task |> Task.run(c => {
      c |> equals(2);
    });
    count^ |> equals(2);
  });

  test("multiple sealed runs", () => {
    let count = ref(0);
    let task = Task.makeSealed(resolve => {
      count := count^ + 1;
      resolve(count^);
    });

    task |> Task.run(c => {
      c |> equals(1);
    });
    count^ |> equals(1);

    task |> Task.run(c => {
      c |> equals(1);
    });
    count^ |> equals(1);
  });

});
