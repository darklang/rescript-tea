open Tea
open App

type model = float

type msg =
  | TestMsg
  | Time(float)

let init = () => (0.0, Cmd.none)

let update = (model, x) =>
  /* let () = Js.log ("Update", model) in */

  switch x {
  | TestMsg => (model, Cmd.none)
  | Time(time) => (time, Cmd.none)
  }

let subscriptions = (_: model) =>
  /* let () = Js.log ("Subscriptions", model) in */
  Time.every(~key= "", Time.second, t => Time(t))

let tau = 8.0 *. atan(1.0)

let view = model => {
  open Svg
  open Svg.Attributes
  /* Yes, the Elm example uses the wrong calculation here too, doing the same to match, off by 15s... */
  let angle = tau *. Time.inMinutes(model)
  let handX = Js.Float.toString(50.0 +. 40.0 *. cos(angle))
  let handY = Js.Float.toString(50.0 +. 40.0 *. sin(angle))
  svg(
    list{viewBox("0 0 100 100"), width("300px")},
    list{
      circle(list{cx("50"), cy("50"), r("45"), fill("#0B79CE")}, list{}),
      line(list{x1("50"), y1("50"), x2(handX), y2(handY), stroke("#023963")}, list{}),
    },
  )
}

let main = standardProgram({
  init,
  update,
  view,
  subscriptions,
})
