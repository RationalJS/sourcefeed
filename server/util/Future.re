type map_fun('a) = ('a => unit) => unit;

type t('a) = Future(map_fun('a));

let make = (resolver) => {
  let callbacks = ref([]);
  let data = ref(None);

  resolver(result => switch(data^) {
    | None =>
      data := Some(result);
      callbacks^ |> List.iter(cb => cb(result));
      /* Clean up memory usage */
      callbacks := []
    | Some(_) =>
      () /* Do nothing; theoretically not possible */
  });

  Future(resolve => switch(data^) {
    | Some(result) => resolve(result)
    | None => callbacks := [resolve, ...callbacks^]
  })
};

let value = (x) => make(resolve => resolve(x));


let map = (f, Future(get)) => make(resolve => {
  get(result => resolve(f(result)))
});

let tap = (f, Future(get) as future) => {
  get(f);
  future
};

let get = (f, Future(getFn)) => getFn(f);

/*
 * Because future(result('a,'b)) is so common,
 * Future provides functions to map the inner result directly.
 */
/*open AeroResult;
let mapOk = (f, Future(get)) => Future( resolve =>
  get(result => switch(result) {
    | Ok(value) => resolve @@ Ok(f(value))
    | Error(_) => resolve(result)
  })
);

let mapError = (f, Future(get)) => Future( resolve =>
  get(result => switch(result) {
    | Ok(_) => resolve(result)
    | Error(err) => resolve @@ Error(f(err))
  })
);
*/