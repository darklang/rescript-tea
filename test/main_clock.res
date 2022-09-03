open Tea
open App
open Html

type model = float

type msg =
  | TestMsg
  | Time(float)

let init = () => (Web.Date.now(), Cmd.none)

let update = (model, x) =>
  /* let () = Js.log ("Update", model) in */

  switch x {
  | TestMsg => (model, Cmd.none)
  | Time(time) => (time, Cmd.none)
  }

let subscriptions =(_: model) =>
  /* let () = Js.log ("Subscriptions", model) in */
  Time.every(~key= "", Time.inMilliseconds(16.0), t => Time(t))

let view = model => {
  let ms = int_of_float(mod_float(model, 1000.0))
  let sec' = int_of_float(model /. 1000.0)
  let sec = mod(sec', 60)
  let min' = sec' / 60
  let min = mod(min', 60)
  let hrs' = min' / 60
  let hrs = mod(hrs', 24)
  span(
    list{},
    list{
      text(
        string_of_int(hrs) ++
        (":" ++
        (string_of_int(min) ++ (":" ++ (string_of_int(sec) ++ ("." ++ string_of_int(ms)))))),
      ),
    },
  )
}

let main = standardProgram({
  init: init,
  update: update,
  view: view,
  subscriptions: subscriptions,
})
