open Tea
open Html
open Html.Events

type msg =
  | Increment
  | Decrement
  | Reset
  | Set(int)
  | OnUrlChange(option<int>)

let toUrl = count => "#/" ++ string_of_int(count)

let fromUrl = url =>
  try Some(int_of_string(String.sub(url, 2, String.length(url) - 2))) catch {
  | _ => None
  }

let update = (model, x) =>
  switch x {
  | Increment => (model + 1, Navigation.newUrl(toUrl(model + 1)))
  | Decrement => (model - 1, Navigation.newUrl(toUrl(model - 1)))
  | Reset => (0, Navigation.newUrl(toUrl(0)))
  | Set(v) => (v, Navigation.newUrl(toUrl(v)))
  | OnUrlChange(loc) =>
    switch loc {
    | None => (0, Navigation.modifyUrl(toUrl(0)))
    | Some(v) => (v, Cmd.none)
    }
  }

let view_button = (title, msg) => button(list{onClick(msg)}, list{text(title)})

let view = model =>
  div(
    list{},
    list{
      span(list{Html.Attributes.style("text-weight", "bold")}, list{text(string_of_int(model))}),
      br(list{}),
      view_button("Increment", Increment),
      br(list{}),
      view_button("Decrement", Decrement),
      br(list{}),
      view_button("Set to 42", Set(42)),
      br(list{}),
      if model != 0 {
        view_button("Reset", Reset)
      } else {
        noNode
      },
    },
  )

let locationToMessage = location => {
  open Web.Location
  OnUrlChange(fromUrl(location.hash))
}

let init = ((), location) => {
  open Web.Location
  switch fromUrl(location.hash) {
  | None => (0, Navigation.modifyUrl(toUrl(0)))
  | Some(v) => (v, Cmd.none)
  }
}

let main = {
  open Navigation
  navigationProgram(
    locationToMessage,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: _model => Sub.none,
      shutdown: _model => Cmd.none,
    },
  )
}
