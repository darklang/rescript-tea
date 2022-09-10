type debugMsg<'msg> = ClientMsg('msg) | TogglePaused | SelectHistoryItem(int) | ToggleDetails

let clientMsg: 'msg => debugMsg<'msg>

type state = Running | Paused(int)
type debug_model<'model> = {history: list<(string, 'model)>, state: state, show_details: bool}

let debug: (
  'msg => string,
  ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  'model => Vdom.t<'msg>,
  'model => Tea_sub.t<'msg>,
  'model => Tea_cmd.t<'msg>,
) => (
  (('model, Tea_cmd.t<'msg>)) => (debug_model<'model>, Tea_cmd.t<debugMsg<'msg>>),
  (debug_model<'model>, debugMsg<'msg>) => (debug_model<'model>, Tea_cmd.t<debugMsg<'msg>>),
  debug_model<'model> => Vdom.t<debugMsg<'msg>>,
  debug_model<'model> => Tea_sub.t<debugMsg<'msg>>,
  debug_model<'model> => Tea_cmd.t<debugMsg<'msg>>,
)

let debugProgram: ('msg => string, Tea_app.program<'flags, 'model, 'msg>) => Tea_app.program<
  'flags,
  debug_model<'model>,
  debugMsg<'msg>,
>

let debugNavigationProgram: (
  'msg => string,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
) => Tea_navigation.navigationProgram<'flags, debug_model<'model>, debugMsg<'msg>>

let beginnerProgram: (
  Tea_app.beginnerProgram<'model, 'msg>,
  'msg => string,
  Js.null_undefined<Web.Node.t>,
  unit,
) => Tea_app.programInterface<debugMsg<'msg>>

let standardProgram: (
  Tea_app.standardProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Web.Node.t>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>>

let program: (
  Tea_app.program<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Web.Node.t>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>>

let navigationProgram: (
  Web.Location.location => 'msg,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Web.Node.t>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>>
