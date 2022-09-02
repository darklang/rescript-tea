/* https://github.com/evancz/react-angular-ember-elm-performance-comparison/blob/master/implementations/elm-0.17/Todo.elm */
open Tea
open App
open Html
open Html.Events


module Cmds = Tea_html_cmds
/* MODEL */

type entry = {
  description: string,
  completed: bool,
  editing: bool,
  id: int,
}

/* The full application state of our todo app. */
type model = {
  entries: array<entry>,
  field: string,
  uid: int,
  visibility: string,
}

let emptyModel = {
  entries: [],
  visibility: "All",
  field: "",
  uid: 0,
}

let newEntry = (desc, id) => {
  description: desc,
  completed: false,
  editing: false,
  id: id,
}

let init = () => (emptyModel, Cmd.none)

/* UPDATE */

/* Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them. */
type msg =
  | NoOp
  | UpdateField(string)
  | EditingEntry(int, bool)
  | UpdateEntry(int, string)
  | Add
  | Delete(int)
  | DeleteComplete
  | Check(int, bool)
  | CheckAll(bool)
  | ChangeVisibility(string)

/* How we update our Model on a given Msg? */
let update = (model, x) =>
  switch x {
  | NoOp => (model, Cmd.none)

  | Add => (
      {
        ...model,
        uid: model.uid + 1,
        field: "",
        entries: if model.field == "" {
          model.entries
        } else {
          Array.concat(list{model.entries, [newEntry(model.field, model.uid)]})
        },
      },
      Cmd.none,
    )

  | UpdateField(field) => ({...model, field: field}, Cmd.none)

  | EditingEntry(id, editing) =>
    let updateEntry = t =>
      if t.id == id {
        {...t, editing: editing}
      } else {
        t
      }
    (
      {
        ...model,
        entries: Array.map(updateEntry, model.entries),
      },
      if editing {
        Cmds.focus("todo-" ++ string_of_int(id))
      } else {
        Cmd.none
      },
    )

  | UpdateEntry(id, description) =>
    let updateEntry = t =>
      if t.id == id {
        {...t, description: description}
      } else {
        t
      }
    (
      {
        ...model,
        entries: Array.map(updateEntry, model.entries),
      },
      Cmd.none,
    )

  | Delete(id) => (
      {
        ...model,
        entries: List.filter(t => t.id != id, Array.to_list(model.entries)) |> Array.of_list,
      },
      Cmd.none,
    )

  | DeleteComplete => (
      {
        ...model,
        entries: List.filter(
          ({completed, _}) => !completed,
          Array.to_list(model.entries),
        ) |> Array.of_list,
      },
      Cmd.none,
    )

  | Check(id, completed) =>
    let updateEntry = t =>
      if t.id == id {
        {...t, completed: completed}
      } else {
        t
      }
    (
      {
        ...model,
        entries: Array.map(updateEntry, model.entries),
      },
      Cmd.none,
    )

  | CheckAll(completed) =>
    let updateEntry = t => {...t, completed: completed}
    (
      {
        ...model,
        entries: Array.map(updateEntry, model.entries),
      },
      Cmd.none,
    )
  | ChangeVisibility(visibility) => ({...model, visibility: visibility}, Cmd.none)
  }

/* View rendering */

let onEnter = (~key="", msg) => {
  let tagger = ev =>
    switch ev["keyCode"] {
    | 13 => Some(msg)
    | _ => None
    }

  onCB("keydown", ~key, tagger)
}

let viewEntry = todo => {
  let key = string_of_int(todo.id)
  li(
    list{Html.Attributes.classList(list{("completed", todo.completed), ("editing", todo.editing)})},
    list{
      div(
        list{Html.Attributes.class("view")},
        list{
          input'(
            list{
              Html.Attributes.class("toggle"),
              Html.Attributes.type'("checkbox"),
              Html.Attributes.checked(todo.completed),
              onClick(Check(todo.id, !todo.completed)),
            },
            list{},
          ),
          label(
            list{onDoubleClick(EditingEntry(todo.id, true))},
            list{text(todo.description)},
          ),
          button(list{Html.Attributes.class("destroy"), onClick(Delete(todo.id))}, list{}),
        },
      ),
      input'(
        list{
          Html.Attributes.class("edit"),
          Html.Attributes.value(todo.description),
          Html.Attributes.name("title"),
          Html.Attributes.id("todo-" ++ string_of_int(todo.id)),
          onInput(~key, value => UpdateEntry(todo.id, value)),
          onBlur(EditingEntry(todo.id, false)),
          onEnter(~key, EditingEntry(todo.id, false)),
        },
        list{},
      ),
    },
  )
}

let viewEntries = (visibility, entries) => {
  let isVisible = todo =>
    switch visibility {
    | "Completed" => todo.completed
    | "Active" => !todo.completed
    | _ => true
    }
  let allCompleted = Array.fold_left((b, {completed, _}) => b && completed, true, entries)
  let cssVisibility = if Array.length(entries) == 0 {
    "hidden"
  } else {
    "visible"
  }
  section(
    list{Html.Attributes.class("main"), Html.Attributes.style("visibility", cssVisibility)},
    list{
      input'(
        list{
          Html.Attributes.class("toggle-all"),
          Html.Attributes.type'("checkbox"),
          Html.Attributes.name("toggle"),
          Html.Attributes.checked(allCompleted),
          onClick( CheckAll(!allCompleted)),
        },
        list{},
      ),
      label(list{Html.Attributes.for'("toggle-all")}, list{text("Mark all as complete")}),
      ul(
        list{Html.Attributes.class("todo-list")},
        List.map(viewEntry, List.filter(isVisible, Array.to_list(entries))),
      ),
    },
  )
}

let viewInput = task =>
  header(
    list{Html.Attributes.class("header")},
    list{
      h1(list{}, list{text("todos")}),
      input'(
        list{
          Html.Attributes.class("new-todo"),
          Html.Attributes.placeholder("What needs to be done?"),
          Html.Attributes.autofocus(true),
          Html.Attributes.value(task),
          Html.Attributes.name("newTodo"),
          onInput(str => UpdateField(str)),
          onEnter(Add),
        },
        list{},
      ),
    },
  )

let viewControlsCount = entriesLeft => {
  let item_ = if entriesLeft === 1 {
    " item"
  } else {
    " items"
  }
  span(
    list{Html.Attributes.class("todo-count")},
    list{strong(list{}, list{text(string_of_int(entriesLeft))}), text(item_ ++ " left")},
  )
}

let visibilitySwap = (uri, visibility, actualVisibility) =>
  li(
    list{onClick(ChangeVisibility(visibility))},
    list{
      a(
        list{Html.Attributes.href(uri), Html.Attributes.classList(list{("selected", visibility == actualVisibility)})},
        list{text(visibility)},
      ),
    },
  )

let viewControlsFilters = visibility =>
  ul(
    list{Html.Attributes.class("filters")},
    list{
      visibilitySwap("#/", "All", visibility),
      text(" "),
      visibilitySwap("#/active", "Active", visibility),
      text(" "),
      visibilitySwap("#/completed", "Completed", visibility),
    },
  )

let viewControlsClear = entriesCompleted =>
  button(
    list{Html.Attributes.class("clear-completed"), Html.Attributes.hidden(entriesCompleted === 0), onClick(DeleteComplete)},
    list{text("Clear completed (" ++ (string_of_int(entriesCompleted) ++ ")"))},
  )

let viewControls = (visibility, entries) => {
  let entriesCompleted = Array.fold_left((c, {completed, _}) =>
    if completed {
      c + 1
    } else {
      c
    }
  , 0, entries)
  let entriesLeft = Array.length(entries) - entriesCompleted
  footer(
    list{Html.Attributes.class("footer"), Html.Attributes.hidden(Array.length(entries) == 0)},
    list{
      viewControlsCount(entriesLeft),
      viewControlsFilters(visibility),
      viewControlsClear(entriesCompleted),
    },
  )
}

let infoFooter = footer(
  list{Html.Attributes.class("info")},
  list{
    p(list{}, list{text("Double-click to edit a todo")}),
    p(
      list{},
      list{
        text("Written by "),
        a(list{Html.Attributes.href("https://github.com/evancz")}, list{text("Evan Czaplicki")}),
        text(" and converted by "),
        a(list{Html.Attributes.href("https://github.com/overminddl1")}, list{text("OvermindDL1")}),
      },
    ),
    p(list{}, list{text("Part of "), a(list{Html.Attributes.href("http://todomvc.com")}, list{text("TodoMVC")})}),
  },
)

let view = model =>
  div(
    list{Html.Attributes.class("todomvc-wrapper"), Html.Attributes.style("visibility", "hidden")},
    list{
      section(
        list{Html.Attributes.class("todoapp")},
        list{
          viewInput(model.field),
          viewEntries(model.visibility, model.entries),
          viewControls(model.visibility, model.entries),
        },
      ),
      infoFooter,
    },
  )

/* 
let array_viewEntry todo =
  let open Vdom in
  let key = string_of_int todo.id in
  arraynode "" "li" "" ""
    [| classList
        [ ("completed", todo.completed)
        ; ("editing", todo.editing)
        ]
    |]
    [| arraynode "" "div" "" ""
         [| class "view" |]
         [| arraynode "" "input" "" ""
              [| class "toggle"
            ; type' "checkbox"
            ; checked todo.completed
            ; onClick ~key:(key ^ string_of_bool todo.completed) (Check (todo.id, not todo.completed))
              |]
              [||]
        ; arraynode "" "label" "" ""
            [| onDoubleClick ~key:key (EditingEntry (todo.id, true)) |]
            [| text todo.description |]
        ; arraynode "" "button" "" ""
            [| class "destroy"
            ; onClick ~key:key (Delete todo.id)
            |]
            [||]
         |]
    ; arraynode "" "input" "" ""
        [| class "edit"
        ; value todo.description
        ; name "title"
        ; id ("todo-" ^ string_of_int todo.id)
        ; onInput ~key:key (fun value -> UpdateEntry (todo.id, value))
        ; onBlur ~key:key (EditingEntry (todo.id, false))
        ; onEnter ~key:key (EditingEntry (todo.id, false))
        |]
        [||]
    |]

let array_viewEntries visibility entries =
  let open Vdom in
  let isVisible todo =
    match visibility with
    | "Completed" -> todo.completed
    | "Active" -> not todo.completed
    | _ -> true in
  let allCompleted =
    Array.fold_left (fun b {completed} -> b && completed) true entries in
  let cssVisibility =
    if Array.length entries = 0 then "hidden" else "visible" in
  arraynode "" "section" "" ""
    [| class "main"
    ; style "visibility" cssVisibility
    |]
    [| arraynode "" "input" "" ""
        [| class "toggle-all"
        ; type' "checkbox"
        ; name "toggle"
        ; checked allCompleted
        ; onClick ~key:(string_of_bool allCompleted) (CheckAll (not allCompleted))
        |]
        [||]
    ; arraynode "" "label" "" ""
        [| for' "toggle-all" |]
        [| text "Mark all as complete" |]
     ; arraynode "" "ul" "" "" [| class "todo-list" |] (Array.map (fun todo -> if isVisible todo then array_viewEntry todo else noNode) entries)
    |]

let array_viewInput task =
  let open Vdom in
  arraynode "" "header" "" ""
    [| class "header" |]
    [| arraynode "" "h1" "" "" [||] [| text "todos" |]
    ; arraynode "" "input" "" ""
        [| class "new-todo"
        ; placeholder "What needs to be done?"
        ; autofocus true
        ; value task
        ; name "newTodo"
        ; onInput (fun str -> (UpdateField str))
        ; onEnter Add
        |]
        [||]
    |]

let array_viewControlsCount entriesLeft =
  let open Vdom in
  let item_ =
    if entriesLeft == 1 then " item" else " items" in
  arraynode "" "span" "" ""
    [| class "todo-count" |]
    [| arraynode "" "strong" "" "" [||] [| text (string_of_int entriesLeft) |]
    ; text (item_ ^ " left")
    |]

let array_visibilitySwap uri visibility actualVisibility =
  let open Vdom in
  arraynode "" "li" "" ""
    [| onClick ~key:visibility (ChangeVisibility visibility) |]
    [| arraynode "" "a" "" "" [| href uri; classList [("selected", visibility = actualVisibility)] |]
         [| text visibility |]
    |]

let array_viewControlsFilters visibility =
  let open Vdom in
  arraynode "" "ul" "" ""
    [| class "filters" |]
    [| visibilitySwap "#/" "All" visibility
    ; text " "
    ; visibilitySwap "#/active" "Active" visibility
    ; text " "
    ; visibilitySwap "#/completed" "Completed" visibility
    |]

let array_viewControlsClear entriesCompleted =
  let open Vdom in
  arraynode "" "button" "" ""
    [| class "clear-completed"
    ; hidden (entriesCompleted == 0)
    ; onClick DeleteComplete
    |]
    [| text ("Clear completed (" ^ string_of_int entriesCompleted ^ ")")
    |]

let array_viewControls visibility entries =
  let open Vdom in
  let entriesCompleted =
    Array.fold_left (fun c {completed} -> if completed then c + 1 else c) 0 entries in
  let entriesLeft =
    Array.length entries - entriesCompleted in
  arraynode "" "footer" "" ""
    [| class "footer"
    ; hidden (Array.length entries = 0)
    |]
    [| array_viewControlsCount entriesLeft
    ; array_viewControlsFilters visibility
    ; array_viewControlsClear entriesCompleted
    |]

let array_infoFooter =
  let open Vdom in
  arraynode "" "footer" "" "" [| class "info" |]
    [| arraynode "" "p" "" "" [||] [| text "Double-click to edit a todo" |]
     ; arraynode "" "p" "" "" [||]
         [| text "Written by "
          ; arraynode "" "a" "" "" [| href "https://github.com/evancz" |] [| text "Evan Czaplicki" |]
        ; text " and converted by "
        ; arraynode "" "a" "" "" [| href "https://github.com/overminddl1" |] [| text "OvermindDL1" |]
         |]
     ; arraynode "" "p" "" "" [||]
         [| text "Part of "
          ; arraynode "" "a" "" "" [| href "http://todomvc.com" |] [| text "TodoMVC" |]
         |]
    |]
let view_array model =
  let open Vdom in
  arraynode "" "div" "" ""
    [| class "todomvc-wrapper"
    ;  style "visibility" "hidden"
    |]
    [| arraynode "" "section" "" ""
        [| class "todoapp" |]
        [| array_viewInput model.field
        ; array_viewEntries model.visibility model.entries
        ; array_viewControls model.visibility model.entries
        |]
    ; array_infoFooter
    |] */

/* let s = ref noNode */
let viewNew = model =>
  /* let () = s := view_array model in */
  view(model)

/* Main Entrance */

let main = standardProgram({
  init: init,
  update: update,
  view: view /* = viewNew */,
  subscriptions: _model => Sub.none,
})
