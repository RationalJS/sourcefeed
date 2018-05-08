let component = ReasonReact.statelessComponent("ProjectPage");

let make = (_children) => {
  ...component,
  render: _self =>
    <button>
      {ReasonReact.string("Hello! (project page)")}
    </button>
};
