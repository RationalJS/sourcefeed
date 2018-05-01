type run_fun('a) = ('a => unit) => unit;
type ready = Ready;
type sealed = Sealed;
type t('a,'state) =
  | Task(run_fun('a)) : t('a, ready)
  | SealedTask(run_fun('a)) : t('a, sealed);

let make = (run) => Task(run);
let value = (x) => Task(resolve => resolve(x));

let map = (type state, f, task : t('a,state)) : t('b,ready) => switch(task) {
  | Task(run) =>
    Task( resolve =>
      run(result => f(result) |> resolve)
    )
  | SealedTask(run) =>
    Task( resolve =>
      run(result => f(result) |> resolve)
    )
};

let flatMap = (type state, type state2,
  f : ('a => t('b,state2)), task : t('a,state)) : t('b,ready)
=>
switch(task) {
  | Task(run) =>
    Task( resolve =>
      run(result => switch(f(result)) {
        | Task(run2) => run2(result2 => result2 |> resolve)
        | SealedTask(run2) => run2(result2 => result2 |> resolve)
      })
    )
  | SealedTask(run) =>
    Task( resolve =>
      run(result => switch(f(result)) {
        | Task(run2) => run2(result2 => result2 |> resolve)
        | SealedTask(run2) => run2(result2 => result2 |> resolve)
      })
    )
};

let run = (type state, receiver, task : t('a,state)) => switch(task) {
  | Task(run) => run(receiver)
  | SealedTask(run) => run(receiver)
};

let seal = (Task(run)) => {
  let callbacks = ref([]);
  let data = ref(None);

  run(result => switch(data^) {
    | None =>
      data := Some(result);
      callbacks^ |> List.iter(cb => cb(result));
      /* Clean up memory usage */
      callbacks := []
    | Some(_) =>
      () /* Do nothing */
  });

  SealedTask(resolve => switch(data^) {
    | Some(result) => resolve(result)
    | None => callbacks := [resolve, ...callbacks^]
  })
};

let makeSealed = (run) => Task(run) |> seal;

/*
 * Because task(result('a,'b)) is so common,
 * Task provides functions to map the inner result directly.
 */
open AeroResult;
let mapOk = (f, Task(run)) => Task( resolve =>
  run(result => switch(result) {
    | Ok(value) => resolve @@ Ok(f(value))
    | Error(_) => resolve(result)
  })
);

let mapError = (f, Task(run)) => Task( resolve =>
  run(result => switch(result) {
    | Ok(_) => resolve(result)
    | Error(err) => resolve @@ Error(f(err))
  })
);
