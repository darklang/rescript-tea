type debugMsg<'msg> =
  | ClientMsg('msg)
  | TogglePaused
  | SelectHistoryItem(int)
  | ToggleDetails
let clientMsg = msg => ClientMsg(msg)

type state =
  | Running
  | Paused(int)

type debugModel<'model> = {
  history: list<(string, 'model)>,
  state: state,
  showDetails: bool,
}

let debug = (
  msgToString: 'msg => string,
  update: ('model, 'msg) => ('model, Tea_cmd.t<'msg>),
  view: 'model => Vdom.t<'msg>,
  subscriptions: 'model => Tea_sub.t<'msg>,
  shutdown: 'model => Tea_cmd.t<'msg>,
): (
  (('model, Tea_cmd.t<'msg>)) => (debugModel<'model>, Tea_cmd.t<debugMsg<'msg>>),
  (debugModel<'model>, debugMsg<'msg>) => (debugModel<'model>, Tea_cmd.t<debugMsg<'msg>>),
  debugModel<'model> => Vdom.t<debugMsg<'msg>>,
  debugModel<'model> => Tea_sub.t<debugMsg<'msg>>,
  debugModel<'model> => Tea_cmd.t<debugMsg<'msg>>,
) => {
  let initDebug = ((cmodel, cmd)) => (
    {
      history: list{("_init_", cmodel)},
      state: Running,
      showDetails: false,
    },
    cmd |> Tea_cmd.map(clientMsg),
  )

  let update' = (model, x) =>
    switch x {
    | ClientMsg(msg) =>
      if model.state == Running {
        let (_, cmodel) = List.hd(model.history)
        let (cmodel', cmd) = update(cmodel, msg)
        let dmodel' = {...model, history: list{(msgToString(msg), cmodel'), ...model.history}}
        (dmodel', cmd |> Tea_cmd.map(clientMsg))
      } else {
        (model, Tea_cmd.none)
      }
    | TogglePaused =>
      switch model.state {
      | Paused(_) => ({...model, state: Running}, Tea_cmd.none)
      | Running => ({...model, state: Paused(0)}, Tea_cmd.none)
      }
    | SelectHistoryItem(i) => ({...model, state: Paused(i)}, Tea_cmd.none)
    | ToggleDetails => ({...model, showDetails: !model.showDetails}, Tea_cmd.none)
    }

  let viewStyles = () => {
    open Tea_html
    let rule = (selector, properties) =>
      properties
      |> List.map(((k, v)) => k ++ (":" ++ v))
      |> String.concat(";")
      |> (x => j`$(selector) {$(x)}`)
      |> text

    node(
      "style",
      list{},
      list{
        rule(
          "#debug.paused",
          list{
            ("position", "fixed"),
            ("top", "0"),
            ("left", "0"),
            ("width", "100%"),
            ("height", "100%"),
            ("pointer-events", "all"),
            ("background-color", "rgba(0,0,0,.1)"),
            ("box-shadow", "inset 0 0 10px #333"),
          },
        ),
        rule(
          "#debug nav",
          list{
            ("position", "fixed"),
            ("max-width", "50%"),
            ("bottom", "0"),
            ("right", "6px"),
            ("border-radius", "4px 4px 0 0"),
            ("background-color", "#444"),
            ("color", "#fff"),
            ("font-family", "monospace"),
            ("box-shadow", "0 0 10px #333"),
          },
        ),
        rule("#debug.paused nav", list{("height", "50%"), ("padding-bottom", "2em")}),
        rule(
          "#debug nav .toggle",
          list{
            ("padding", "6px"),
            ("padding-left", "9px"),
            ("cursor", "pointer"),
            ("min-width", "24ch"),
            ("text-align", "center"),
            ("border-left", "3px solid #333"),
            ("border-radius", "4px 4px 0 0"),
          },
        ),
        rule(
          "#debug nav .toggle:before",
          list{
            ("content", "' '"),
            ("position", "absolute"),
            ("left", "0"),
            ("top", "0"),
            ("width", ".5ch"),
            ("height", "1.8ch"),
            ("margin", "1.2ch"),
            ("border", "solid #fff"),
            ("border-width", "0 .5ch"),
          },
        ),
        rule(
          "#debug.paused nav .toggle:before",
          list{
            ("border-color", "transparent"),
            ("border-left-color", "#fff"),
            ("border-width", "1ch"),
            ("width", "0"),
            ("height", "0"),
          },
        ),
        rule(
          "#debug nav .history",
          list{
            ("margin", "0"),
            ("padding", "0"),
            ("height", "100%"),
            ("overflow-y", "auto"),
            ("list-style", "none"),
          },
        ),
        rule(
          "#debug nav .history li",
          list{("margin", "0"), ("padding", "0.2ch"), ("border-left", "3px solid #333")},
        ),
        rule("#debug nav .history li.selected", list{("background-color", "#333")}),
        rule(
          "#debug nav .history span.details",
          list{
            ("display", "inline-block"),
            ("cursor", "pointer"),
            ("width", "1ch"),
            ("margin", "0 1ch"),
            ("vertical-align", "super"),
          },
        ),
        rule("#debug nav .history li.selected span.details:after", list{("content", "'\\2026'")}),
        rule("#debug nav .history li.selected.show", list{("border-left", "3px solid white")}),
        rule(
          "#debug nav .history span.message",
          list{
            ("display", "inline-block"),
            ("cursor", "pointer"),
            ("white-space", "nowrap"),
            ("overflow", "hidden"),
            ("text-overflow", "ellipsis"),
            ("width", "calc(100% - 75px)"),
          },
        ),
        rule(
          "#debug nav .history span.index",
          list{
            ("display", "inline-block"),
            ("min-width", "3ch"),
            ("margin", "0 1ch"),
            ("color", "#aaa"),
            ("text-align", "right"),
            ("float", "right"),
          },
        ),
        rule(
          "#debug aside.details",
          list{
            ("position", "absolute"),
            ("width", "40ch"),
            ("top", "0"),
            ("bottom", "0"),
            ("right", "100%"),
            ("margin-right", "1.5ch"),
            ("overflow", "scroll"),
            ("background-color", "#fff"),
            ("color", "#000"),
            ("box-shadow", "0 0 10px #333"),
            ("border-radius", "4px 4px 0 0"),
            ("border", "2px solid #333"),
            ("padding", "1ch"),
            ("white-space", "pre"),
          },
        ),
      },
    )
  }

  let viewDetails = model => {
    open Tea_html
    module A = Tea_html.Attributes
    let format = %raw(`
      function (v) {
        var formatRecord = function (data, labels) {
          return data.reduce(
            function (acc, cur, index) {
              acc[labels[index]] = formatValue(cur)
              return acc
            }, {})
        }
        var listToArray = function (data) {
          var result = []
          var cur = data
          while (typeof cur !== "number") {
            result.push(formatValue(cur[0]))
            cur = cur[1]
          }
          return result
        }
        var formatVariant = function (data, recordVariant) {
          if (recordVariant === "::") {
            return listToArray(data)
          }
          else {
            return formatRecord(data, [recordVariant])
          }
        }
        var formatValue = function (x) {
          var recordLabels, recordVariant, recordModule, recordPolyVar
          if (x == null) {
            return null
          }
          else if ((recordLabels = x[Symbol.for('BsRecord')]) !== undefined) {
            return formatRecord(x, recordLabels)
          }
          else if ((recordModule = x[Symbol.for('BsLocalModule')]) !== undefined) {
            return formatRecord(x, recordModule)
          }
          else if ((recordVariant = x[Symbol.for('BsVariant')]) !== undefined) {
            return formatVariant(x, recordVariant)
          }
          else if ((recordPolyVar = x[Symbol.for('BsPolyVar')]) !== undefined) {
            return x[1]
          }
          else if (Array.isArray(x)) {
            // tuple
            return x.map(formatValue)
          }
          else {
            // scalar
            return x
          }
        }
        return JSON.stringify(formatValue(v), null, 2);
      }
    `)
    aside(list{A.class("details")}, list{model |> format |> text})
  }

  let viewHistory = (model, selectedIndex) => {
    open Tea_html
    module A = Tea_html.Attributes
    module E = Tea_html.Events
    let count = List.length(model.history)
    \"@@"(ul(list{A.class("history")}), List.mapi((i, (msg, cmodel)) => {
        let selected = i == selectedIndex
        li(
          list{
            E.onClick(SelectHistoryItem(i)),
            A.classList(list{("selected", selected), ("show", selected && model.showDetails)}),
          },
          list{
            span(
              list{
                A.classList(list{("details", true), ("show", true)}),
                ...if selected {
                  list{E.onClick(ToggleDetails), A.title("toggle details")}
                } else {
                  list{A.noProp, A.noProp}
                },
              },
              list{
                if selected && model.showDetails {
                  viewDetails(cmodel)
                } else {
                  noNode
                },
              },
            ),
            span(list{A.class("message")}, list{text(msg)}),
            span(list{A.class("index")}, list{(count - i)->Belt.Int.toString->text}),
          },
        )
      }, model.history))
  }

  let view' = model => {
    open Tea_html
    module A = Tea_html.Attributes
    module E = Tea_html.Events
    let (selectedIndex, selectedModel, paused) = switch model.state {
    | Running => (0, List.hd(model.history)->snd, false)
    | Paused(index) => (index, List.nth(model.history, index)->snd, true)
    }

    let historyCount = List.length(model.history)
    div(
      list{},
      list{
        view(selectedModel)->Tea_app.map(clientMsg, _),
        div(
          list{A.id("debug"), A.classList(list{("paused", paused)})},
          list{
            viewStyles(),
            nav(
              list{},
              list{
                div(
                  list{
                    A.class("toggle"),
                    E.onClick(TogglePaused),
                    paused ? A.title("click to resume") : A.title("click to pause"),
                  },
                  list{j`Explore History ($(historyCount))` |> text},
                ),
                if paused {
                  viewHistory(model, selectedIndex)
                } else {
                  noNode
                },
              },
            ),
          },
        ),
      },
    )
  }

  let subscriptions' = model =>
    model.history->List.hd->snd->subscriptions->Tea_sub.map(clientMsg, _)

  let shutdown' = model => model.history->List.hd->snd->shutdown->Tea_cmd.map(clientMsg, _)

  (initDebug, update', view', subscriptions', shutdown')
}

let debugProgram: (
  'msg => string,
  Tea_app.program<'flags, 'model, 'msg>,
) => Tea_app.program<'flags, debugModel<'model>, debugMsg<'msg>> = (
  string_of_msg,
  {init, update, view, subscriptions, shutdown},
) => {
  let (initDebug, update', view', subscriptions', shutdown') = debug(
    string_of_msg,
    update,
    view,
    subscriptions,
    shutdown,
  )

  {
    init: flags => init(flags)->initDebug,
    update: update',
    view: view',
    subscriptions: subscriptions',
    shutdown: shutdown',
  }
}

let debugNavigationProgram: (
  'msg => string,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
) => Tea_navigation.navigationProgram<'flags, debugModel<'model>, debugMsg<'msg>> = (
  string_of_msg,
  {init, update, view, subscriptions, shutdown},
) => {
  let (initDebug, update', view', subscriptions', shutdown') = debug(
    string_of_msg,
    update,
    view,
    subscriptions,
    shutdown,
  )

  {
    init: (flags, location) => init(flags, location) |> initDebug,
    update: update',
    view: view',
    subscriptions: subscriptions',
    shutdown: shutdown',
  }
}

let beginnerProgram: (
  Tea_app.beginnerProgram<'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  unit,
) => Tea_app.programInterface<debugMsg<'msg>> = (
  {model, update, view},
  string_of_msg,
  pnode,
  flags,
) => {
  let debugged = debugProgram(
    string_of_msg,
    {
      init: () => (model, Tea_cmd.none),
      update: (model, msg) => (update(model, msg), Tea_cmd.none),
      view: view,
      subscriptions: _model => Tea_sub.none,
      shutdown: _model => Tea_cmd.none,
    },
  )
  Tea_app.program(debugged, pnode, flags)
}

let standardProgram: (
  Tea_app.standardProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>> = (
  {init, update, view, subscriptions},
  string_of_msg,
  pnode,
  flags,
) => {
  let debugged = debugProgram(
    string_of_msg,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: _model => Tea_cmd.none,
    },
  )
  Tea_app.program(debugged, pnode, flags)
}

let program: (
  Tea_app.program<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>> = (
  {init, update, view, subscriptions, shutdown},
  string_of_msg,
  pnode,
  flags,
) => {
  let debugged = debugProgram(
    string_of_msg,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: shutdown,
    },
  )
  Tea_app.program(debugged, pnode, flags)
}

let navigationProgram: (
  Tea_navigation.Location.t => 'msg,
  Tea_navigation.navigationProgram<'flags, 'model, 'msg>,
  'msg => string,
  Js.null_undefined<Dom.node>,
  'flags,
) => Tea_app.programInterface<debugMsg<'msg>> = (
  locationToMsg,
  {init, update, view, subscriptions, shutdown},
  msgToString,
  pnode,
  flags,
) => {
  let location = location => location->locationToMsg->clientMsg

  let debugged = debugNavigationProgram(
    msgToString,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: shutdown,
    },
  )
  Tea_navigation.navigationProgram(location, debugged, pnode, flags)
}
