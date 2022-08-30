open Tea
open App
open Html
open Tea_html.Events

type model = {dieFace: int}

type msg =
  | Roll
  | NewFace(int)

let init = () => ({dieFace: 1}, Cmd.none)

let update = (model, x) =>
  switch x {
  | Roll => (model, Tea.Random.generate(v => NewFace(v), Tea.Random.int(1, 6)))
  | NewFace(dieFace) => ({dieFace: dieFace}, Cmd.none)
  }

let subscriptions = _model => Sub.none

let view = model =>
  div(
    list{},
    list{
      h1(list{}, list{text(string_of_int(model.dieFace))}),
      button(list{onClick(Roll)}, list{text("Roll")}),
    },
  )

let main = standardProgram({
  init: init,
  update: update,
  view: view,
  subscriptions: subscriptions,
})
