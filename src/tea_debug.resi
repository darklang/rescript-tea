type debugMsg<'msg> =
  | ClientMsg('msg)
  | TogglePaused
  | SelectHistoryItem(int)
  | ToggleDetails

let clientMsg: 'msg => debugMsg<'msg>

type state = Running | Paused(int)

type debugModel<'model> = {
  history: list<(string, 'model)>,
  state: state,
  showDetails: bool,
}

let debug: (
  'msg => string,
  ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  'model => Vdom.t<'msg>,
  'model => Tea_sub.t<'msg>,
  'model => Tea_cmd.t<'msg>,
) => (
  (('model, Tea_cmd.t<'msg>)) => (debugModel<'model>, Tea_cmd.t<debugMsg<'msg>>),
  (debugModel<'model>, debugMsg<'msg>) => (debugModel<'model>, Tea_cmd.t<debugMsg<'msg>>),
  debugModel<'model> => Vdom.t<debugMsg<'msg>>,
  debugModel<'model> => Tea_sub.t<debugMsg<'msg>>,
  debugModel<'model> => Tea_cmd.t<debugMsg<'msg>>,
)

let debugProgram: (
  'msg => string,
  Tea_app.program<'flags, 'model, 'msg>,
) => Tea_app.program<'flags, debugModel<'model>, debugMsg<'msg>>

let debugNavigationProgram: (
  'msg => string,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
) => Tea_navigation.navigationProgram<'flags, debugModel<'model>, debugMsg<'msg>>

let beginnerProgram: (
  Tea_app.beginnerProgram<'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  unit,
) => Tea_app.programInterface<debugMsg<'msg>>

let standardProgram: (
  Tea_app.standardProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>>

let program: (
  Tea_app.program<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>>

let navigationProgram: (
  Tea_navigation.Location.t => 'msg,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>>
