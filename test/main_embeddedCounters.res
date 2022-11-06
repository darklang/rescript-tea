open Tea.App
open Tea.Html
open Tea.Html.Events

type msg =
  | Counter(int, Counter.msg)
  | AddCounter
  | RemoveCounter

type model = {counters: list<Counter.model>}

let update = (model, x) =>
  switch x {
  | Counter(idx, ms) =>
    let () = Js.log((model, idx, ms))
    {
      /* model with */
      counters: model.counters |> List.mapi((i, m) =>
        if i != idx {
          m
        } else {
          Counter.update(m, ms)
        }
      ),
    }
  | AddCounter => {
      /* model with */
      counters: list{Counter.init(), ...model.counters},
    }
  | RemoveCounter => {
      /* model with */
      counters: List.tl(model.counters),
    }
  }

let view_button = (title, msg) => button(list{onClick(msg)}, list{text(title)})

let view = model =>
  div(
    list{},
    list{
      button(list{onClick(AddCounter)}, list{text("Prepend a Counter")}),
      if List.length(model.counters) == 0 {
        noNode
      } else {
        button(list{onClick(RemoveCounter)}, list{text("Delete a Counter")})
      },
      div(list{}, List.mapi((i, mo) => Counter.view(ms => Counter(i, ms), mo), model.counters)),
    },
  )

let main = beginnerProgram({
  model: {counters: list{}},
  update: update,
  view: view,
})
