module Router = ReasonReact.Router;

type react_component = Js.t({ .
  [@bs.meth]
  make: unit => ReasonReact.reactElement
});

[@bs.val] external import : string => Js.Promise.t(react_component) = "";

Js.log("ClientRouter .");

let router = (url : Router.url) => {
  Js.log2("The route", url);
  switch (url.path) {
  | [] =>
    /* LAST TIME: WAITING ON https://github.com/fastpack/fastpack/issues/81 */
    import("/assets/IndexPage/index.js")
    |> Js.Promise.then_(m => {
      ReactDOMRe.renderToElementWithId(m##make(), "app");
      Js.Promise.resolve()
    })
    |> ignore
  | ["projects", id] =>
    ReactDOMRe.renderToElementWithId(<p>{ReasonReact.string("Project "++id)}</p>, "app")
  | _ =>
    ReactDOMRe.renderToElementWithId(<p>{ReasonReact.string("Not found.")}</p>, "app")
  }
};

/* Kick things off */
Router.dangerouslyGetInitialUrl() |> router;

/* Watch for pushstate changes */
let watcherID = Router.watchUrl(router);
