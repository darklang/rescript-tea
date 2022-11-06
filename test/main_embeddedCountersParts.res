open Tea
open App
open Html
open Tea.Html.Events

type msg =
  | Counters(CounterParts.msg)
  | AddCounter
  | RemoveCounter

type model = {
  counters: CounterParts.model<msg>,
  count: int,
}

let init = () => (
  {
    counters: CounterParts.init(4, sm => Counters(sm)),
    count: 0,
  },
  Cmd.none,
)

let update = (model, x) =>
  switch x {
  | Counters(cMsg) =>
    let () = Js.log((model, cMsg))
    (
      {
        ...model,
        counters: CounterParts.update(model.counters, cMsg),
      },
      Cmd.none,
    )
  | AddCounter => (
      {
        ...model,
        count: model.count + 1,
      },
      Cmd.none,
    )
  | RemoveCounter => (
      {
        ...model,
        count: model.count - 1,
      },
      CounterParts.shutdown(model.counters, model.count),
    )
  }

let view_button = (title, msg) => button(list{onClick(msg)}, list{text(title)})

let view = model => {
  let showCounter = () => {
    let rec showCountere_rec = (l, a, b) =>
      if a > b {
        l
      } else {
        showCountere_rec(list{CounterParts.view(b, model.counters), ...l}, a, b - 1)
      }
    showCountere_rec(list{}, 1, model.count)
  }
  div(
    list{},
    list{
      button(list{onClick(AddCounter)}, list{text("Append a Counter")}),
      if model.count == 0 {
        noNode
      } else {
        button(list{onClick(RemoveCounter)}, list{text("Delete a Counter")})
      },
      div(list{}, showCounter()),
    },
  )
}

let main = standardProgram({
  init: init,
  update: update,
  view: view,
  subscriptions: _model => Sub.none,
})
