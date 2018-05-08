let component = ReasonReact.statelessComponent("IndexPage");

let make = (_children) => {
  ...component,
  render: _self =>
    <button>
      {ReasonReact.string("Hello! (index page)")}
    </button>
};
