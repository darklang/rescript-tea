open Tea
open App
open Html
open Tea_html.Events

type model = {
  r: int,
  g: int,
  b: int,
}

type msg =
  | Roll
  | NewColor(int, int, int)

let init = () => ({r: 255, g: 255, b: 255}, Cmd.none)

let update = (model, x) =>
  switch x {
  | Roll =>
    let genInt = Tea.Random.int(1, 255)
    /* Here is how to do it via a list */
    /* let genList3 = Tea.Random.list 3 genInt in
     model, Tea.Random.generate (function |[r;g;b] -> NewColor (r,g,b) | v -> failwith "will never happen") genList3 */
    /* And here is how to do it via a mapping */
    let genTuple3 = Tea.Random.map3((r, g, b) => NewColor(r, g, b), genInt, genInt, genInt)
    (model, Tea.Random.generate(v => v, genTuple3))
  | NewColor(r, g, b) => ({r: r, g: g, b: b}, Cmd.none)
  }

let subscriptions = _model => Sub.none

let view = model => {
  let r = string_of_int(model.r)
  let g = string_of_int(model.g)
  let b = string_of_int(model.b)
  let isDark = (model.r + model.g + model.b) / 3 > 128
  let rgb = "(" ++ (r ++ ("," ++ (g ++ ("," ++ (b ++ ")")))))
  let altRgb = if isDark {
    "(0,0,0)"
  } else {
    "(255,255,255)"
  }

  div(
    list{},
    list{
      h1(
        list{Tea_html.Attributes.style("background-color", "rgb" ++ rgb), Tea_html.Attributes.style("color", "rgb" ++ altRgb)},
        list{text(rgb)},
      ),
      button(list{onClick(Roll)}, list{text("Roll")}),
    },
  )
}

let main = standardProgram({
  init: init,
  update: update,
  view: view,
  subscriptions: subscriptions,
})
