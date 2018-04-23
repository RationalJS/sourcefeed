type result('r, 'e) =
  | Ok('r)
  | Err('e);

type run_fun('a) = ('a => unit) => unit;
type t('a) = Task(run_fun('a));

let make = (x) => Task(x);

let run = (receiver, Task(run)) => run(receiver);
let map = (f, Task(run)) => Task( resolve =>
  run(result => f(result) |> resolve)
);
let flatMap = (f, Task(run)) => Task( resolve =>
  run(result => switch(f(result)) {
    | Task(run2) => run2(result2 => result2 |> resolve)
  })
);
