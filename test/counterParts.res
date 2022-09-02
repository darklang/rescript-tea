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

let get_value = (id, model) =>
  if IntMap.mem(id, model.values) {
    IntMap.find(id, model.values)
  } else {
    model.defaultValue
  }

let put_value = (id, value, model) => {
  ...model,
  values: IntMap.add(id, value, model.values),
}

let mutate_value = (id, op, model) => {
  let value = get_value(id, model)
  put_value(id, op(value), model)
}

let remove_value = (id, model) => {
  ...model,
  values: IntMap.remove(id, model.values),
}

let update = (model, x) =>
  switch x {
  | Increment(id) => mutate_value(id, \"+"(1), model)
  | Decrement(id) => mutate_value(id, i => i - 1, model)
  | Reset(id) => put_value(id, 0, model)
  | Set(id, v) => put_value(id, v, model)
  | Shutdown(id) => remove_value(id, model)
  }

let shutdown = (model, id) => Cmd.msg(model.lift(Shutdown(id)))

let view_button = (title, msg) => button(list{onClick(msg)}, list{text(title)})

let view = (id, model) => {
  let lift = model.lift
  let value = get_value(id, model)
  div(
    list{Tea.Html.Attributes.styles(list{("display", "inline-block"), ("vertical-align", "top")})},
    list{
      span(list{Tea.Html.Attributes.style("text-weight", "bold")}, list{text(string_of_int(value))}),
      br(list{}),
      view_button("Increment", lift(Increment(id))),
      br(list{}),
      view_button("Decrement", lift(Decrement(id))),
      br(list{}),
      view_button("Set to 42", lift(Set(id, 42))),
      br(list{}),
      if value != 0 {
        view_button("Reset", lift(Reset(id)))
      } else {
        noNode
      },
    },
  )
}
