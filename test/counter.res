open Tea.Html
open Tea.Html.Events


type msg =
  | Increment
  | Decrement
  | Reset
  | Set(int)

type model = int

let init = () => 4

let update = (model, x) =>
  switch x {
  | Increment => model + 1
  | Decrement => model - 1
  | Reset => 0
  | Set(v) => v
  }

let view_button = (title, ~key="", msg) => button(list{onClick'(~key, msg)}, list{text(title)})

let view = (lift, model) =>
  div(
    list{Tea.Html.Attributes.styles(list{("display", "inline-block"), ("vertical-align", "top")})},
    list{
      span(list{Tea.Html.Attributes.style("text-weight", "bold")}, list{text(string_of_int(model))}),
      br(list{}),
      view_button("Increment", lift(Increment)),
      br(list{}),
      view_button("Decrement", lift(Decrement)),
      br(list{}),
      view_button("Set to 42", lift(Set(42))),
      br(list{}),
      if model != 0 {
        view_button("Reset", lift(Reset))
      } else {
        noNode
      },
    },
  )
