type navigationProgram<'flags, 'model, 'msg> = {
  init: ('flags, Web.Location.location) => ('model, Tea_cmd.t<'msg>),
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
}

let getLocation: unit => Web.Location.location

let notifier: ref<option<Web.Location.location => unit>>

let notifyUrlChange: unit => unit

let subscribe: (Web.Location.location => 'msg) => Tea_sub.t<'msg>

let replaceState: string => unit

let pushState: string => unit

let modifyUrl: string => Tea_cmd.t<'msg>

let newUrl: string => Tea_cmd.t<'msg>

let go: int => Tea_cmd.t<'msg>

let back: int => Tea_cmd.t<'msg>

let forward: int => Tea_cmd.t<'msg>

let navigationProgram: (
  Web.Location.location => 'msg,
  navigationProgram<'b, 'c, 'msg>,
  Js.null_undefined<Web.Node.t>,
  'b,
) => Tea_app.programInterface<'msg>
