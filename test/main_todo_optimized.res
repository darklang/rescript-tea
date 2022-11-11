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
  entries: list<entry>,
  field: string,
  uid: int,
  visibility: string,
}

let emptyModel = {
  entries: list{},
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
  /* | NoOp */
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
  /* NoOp -> model, Cmd.none */

  switch x {
  | Add => (
      {
        ...model,
        uid: model.uid + 1,
        field: "",
        entries: if model.field == "" {
          model.entries
        } else {
          list{newEntry(model.field, model.uid), ...model.entries}
        } /* Optimized:  Switched to prepending to a list instead of appending, why the heck would you append only?! */,
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
        entries: List.map(updateEntry, model.entries),
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
        entries: List.map(updateEntry, model.entries),
      },
      Cmd.none,
    )

  | Delete(id) => (
      {
        ...model,
        entries: List.filter(t => t.id != id, model.entries),
      },
      Cmd.none,
    )

  | DeleteComplete => (
      {
        ...model,
        entries: List.filter(({completed, _}) => !completed, model.entries),
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
        entries: List.map(updateEntry, model.entries),
      },
      Cmd.none,
    )

  | CheckAll(completed) =>
    let updateEntry = t => {...t, completed: completed}
    (
      {
        ...model,
        entries: List.map(updateEntry, model.entries),
      },
      Cmd.none,
    )

  | ChangeVisibility(visibility) => ({...model, visibility: visibility}, Cmd.none)
  }

/* View rendering */
let onEnter = (~key="", msg) => {
  let tagger = ev =>
    switch Webapi.Dom.KeyboardEvent.key(Obj.magic(ev)) {
    | "Enter" => Some(msg)
    | _ => None
    }

  onCB("keydown", ~key, tagger)
}

let viewEntry = (todo, ()) => {
  open Html.Attributes
  let key = string_of_int(todo.id)
  let fullkey = key ++ (string_of_bool(todo.completed) ++ string_of_bool(todo.editing))
  /* Optimized:  Added a key to the node to early-out if exact match, if key is unspecified then the sub section is
   always recursed into, thus this is *not* like elm's keyed nodes but rather more strict. */
  li(
    ~key=fullkey,
    list{classList(list{("completed", todo.completed), ("editing", todo.editing)})},
    list{
      div(
        list{class("view")},
        list{
          input'(
            list{
              id("todo-" ++ todo.id->Js.Int.toString),
              class("toggle"),
              type'("checkbox"),
              checked(todo.completed),
              onCheck(v => Check(todo.id, v)),
            },
            list{},
          ),
          label(
            list{
              Html.Attributes.for'("todo-" ++ todo.id->Js.Int.toString),
              onDoubleClick(EditingEntry(todo.id, true)),
            },
            list{text(todo.description)},
          ),
          button(list{class("destroy"), onClick(Delete(todo.id))}, list{"Delete"->text}),
        },
      ),
      input'(
        list{
          class("edit"),
          value(todo.description),
          name("title"),
          id("todo-" ++ string_of_int(todo.id)),
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
  let allCompleted = List.for_all(({completed, _}) => completed, entries)
  let cssVisibility = if list{} == entries {
    "hidden"
  } else {
    "visible"
  }
  section(
    list{Html.Attributes.class("main"), Html.Attributes.style("visibility", cssVisibility)},
    list{
      input'(
        list{
          Html.Attributes.id("toggle-all"),
          Html.Attributes.class("toggle-all"),
          Html.Attributes.type'("checkbox"),
          Html.Attributes.name("toggle"),
          Html.Attributes.checked(allCompleted),
          onCheck(v => CheckAll(v)),
        },
        list{},
      ),
      label(list{Html.Attributes.for'("toggle-all")}, list{text("Mark all as complete")}),
      ul(
        list{Html.Attributes.class("todo-list")},
        List.rev_map(
          todo =>
            lazy1(
              string_of_int(todo.id) ++
              (string_of_bool(todo.completed) ++
              string_of_bool(todo.editing)),
              viewEntry(todo),
            ),
          List.filter(isVisible, entries),
        ),
      ),
    },
  )
}

let viewInput = (task, ()) =>
  header(
    ~key=task,
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
  let left = string_of_int(entriesLeft)
  span(
    ~key=left,
    list{Html.Attributes.class("todo-count")},
    list{strong(list{}, list{text(left)}), text(item_ ++ " left")},
  )
}

let visibilitySwap = (uri, visibility, actualVisibility) =>
  li(
    list{onClick(ChangeVisibility(visibility))},
    list{
      a(
        list{
          Html.Attributes.href(uri),
          Html.Attributes.classList(list{("selected", visibility == actualVisibility)}),
        },
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
    list{
      Html.Attributes.class("clear-completed"),
      Html.Attributes.hidden(entriesCompleted === 0),
      onClick(DeleteComplete),
    },
    list{text("Clear completed (" ++ (string_of_int(entriesCompleted) ++ ")"))},
  )

let viewControls = (visibility, entries) => {
  let entriesCompleted = List.length(List.filter(({completed, _}) => completed, entries))
  let entriesLeft = List.length(entries) - entriesCompleted
  footer(
    list{Html.Attributes.class("footer"), Html.Attributes.hidden(List.length(entries) == 0)},
    list{
      viewControlsCount(entriesLeft),
      viewControlsFilters(visibility),
      viewControlsClear(entriesCompleted),
    },
  )
}

let infoFooter = () =>
  footer(
    ~key="1",
    list{Html.Attributes.class("info")},
    list{
      p(list{}, list{text("Double-click to edit a todo")}),
      p(
        list{},
        list{
          text("Written by "),
          a(list{Html.Attributes.href("https://github.com/evancz")}, list{text("Evan Czaplicki")}),
          text(" and converted by "),
          a(
            list{Html.Attributes.href("https://github.com/overminddl1")},
            list{text("OvermindDL1")},
          ),
        },
      ),
      p(
        list{},
        list{
          text("Part of "),
          a(list{Html.Attributes.href("http://todomvc.com")}, list{text("TodoMVC")}),
        },
      ),
    },
  )

let view = model =>
  div(
    list{Html.Attributes.class("todomvc-wrapper")},
    list{
      section(
        list{Html.Attributes.class("todoapp")},
        /* [ viewInput model.field () */
        /* Optimization: Set the input as a lazy field */
        list{
          lazy1(model.field, viewInput(model.field)),
          viewEntries(model.visibility, model.entries),
          viewControls(model.visibility, model.entries),
        },
      ),
      lazy1("", infoFooter),
    },
  )

/* Main Entrance */

let main = standardProgram({
  init: init,
  update: update,
  view: view,
  subscriptions: _model => Sub.none,
})
