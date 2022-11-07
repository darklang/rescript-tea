open Tea
open Tea_html
open Tea.Html.Events

type msg =
  | Increment(int)
  | Decrement(int)
  | Reset(int)
  | Set(int, int)
  | Shutdown(int)

module IntMap = Map.Make({
  type t = int
  let compare = compare
})

type model<'parentMsg> = {
  values: IntMap.t<int>,
  defaultValue: int,
  lift: msg => 'parentMsg,
}

let init = (defaultValue, lift) => {
  values: IntMap.empty,
  defaultValue: defaultValue,
  lift: lift,
}

let getValue = (id, model) =>
  if IntMap.mem(id, model.values) {
    IntMap.find(id, model.values)
  } else {
    model.defaultValue
  }

let putValue = (id, value, model) => {
  ...model,
  values: IntMap.add(id, value, model.values),
}

let mutateValue = (id, op, model) => {
  let value = getValue(id, model)
  putValue(id, op(value), model)
}

let remove_value = (id, model) => {
  ...model,
  values: IntMap.remove(id, model.values),
}

let update = (model, x) =>
  switch x {
  | Increment(id) => mutateValue(id, \"+"(1), model)
  | Decrement(id) => mutateValue(id, i => i - 1, model)
  | Reset(id) => putValue(id, 0, model)
  | Set(id, v) => putValue(id, v, model)
  | Shutdown(id) => remove_value(id, model)
  }

let shutdown = (model, id) => Cmd.msg(model.lift(Shutdown(id)))

let viewButton = (title, msg) => button(list{onClick(msg)}, list{text(title)})

let view = (id, model) => {
  let lift = model.lift
  let value = getValue(id, model)
  div(
    list{Tea.Html.Attributes.styles(list{("display", "inline-block"), ("vertical-align", "top")})},
    list{
      span(
        list{Tea.Html.Attributes.style("text-weight", "bold")},
        list{text(Belt.Int.toString(value))},
      ),
      br(list{}),
      viewButton("Increment", lift(Increment(id))),
      br(list{}),
      viewButton("Decrement", lift(Decrement(id))),
      br(list{}),
      viewButton("Set to 42", lift(Set(id, 42))),
      br(list{}),
      if value != 0 {
        viewButton("Reset", lift(Reset(id)))
      } else {
        noNode
      },
    },
  )
}
