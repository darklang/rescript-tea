open Tea
open App
open Html
open Html.Attributes
open Html.Events

type mathMod = {
  baseAdd: float,
  mult: float,
}

type upgradeHow =
  | ClickMath(mathMod)
  | AddAutoClicker(mathMod)

type upgradeDef = {
  cost: float,
  name: string,
  math: upgradeHow,
}

type model = {
  startTime: Time.t,
  curTime: Time.t,
  lastUpdated: Time.t,
  credits: float,
  clickWorthMath: mathMod,
  upgradesRemaining: list<upgradeDef>,
  upgradesUsed: int,
  messages: list<string>,
  autoClicker: mathMod,
}

type msg =
  | OnFrame(AnimationFrame.t)
  | Click
  | DoUpgrade(string)

/* Type helpers */

let mathMod = (baseAdd, mult) => {baseAdd: baseAdd, mult: mult}

/* Init functionality */

let init_mathMod = mathMod(1.0, 1.0)

let empty_mathMod = mathMod(0.0, 1.0)

let init_upgrade = (cost, name, math) => {cost: cost, name: name, math: math}

let init_upgrades = List.sort(
  (upgradeL, upgradeR) => int_of_float(upgradeL.cost -. upgradeR.cost),
  /* Keep the names unique as those are the key */
  list{
    init_upgrade(10.0, "1 more credit per click", ClickMath(mathMod(1.0, 1.0))),
    init_upgrade(50.0, "Half a credit more per click", ClickMath(mathMod(0.5, 1.0))),
    init_upgrade(100.0, "50% more credits per click", ClickMath(mathMod(0.0, 1.5))),
    init_upgrade(150.0, "Finally!  An auto-clicker!", AddAutoClicker(mathMod(1.0, 1.0))),
    init_upgrade(200.0, "Upgrade the auto-clicker!", AddAutoClicker(mathMod(0.0, 1.5))),
    init_upgrade(250.0, "Even 50% more credits per click", ClickMath(mathMod(0.0, 1.5))),
    init_upgrade(
      300.0,
      "Get another base half-credit per click please?",
      ClickMath(mathMod(0.5, 1.0)),
    ),
    init_upgrade(350.0, "Ooo, double the click value!", ClickMath(mathMod(0.0, 2.0))),
    init_upgrade(400.0, "Get another auto-clicker?", AddAutoClicker(mathMod(1.0, 1.0))),
    init_upgrade(500.0, "Get yet another auto-clicker!", AddAutoClicker(mathMod(1.0, 1.0))),
    init_upgrade(750.0, "Upgrade the auto-clickers?", AddAutoClicker(mathMod(0.0, 1.5))),
    init_upgrade(1000.0, "Get another base half-credit per click", ClickMath(mathMod(0.5, 1.0))),
    init_upgrade(1250.0, "Found a way to double output of clicks!", ClickMath(mathMod(0.0, 2.0))),
    init_upgrade(1500.0, "Upgrade the auto-clickers much more!", AddAutoClicker(mathMod(0.0, 4.0))),
    init_upgrade(
      2000.0,
      "Both an additional half credit and another 50% more!",
      ClickMath(mathMod(0.5, 1.5)),
    ),
    init_upgrade(5000.0, "Even more 50% more credits per click", ClickMath(mathMod(0.0, 1.5))),
    init_upgrade(10000.0, "*2* base credits per click! Wow!", ClickMath(mathMod(2.0, 1.0))),
    init_upgrade(
      20000.0,
      "Last upgrade! 10x everything!  Please submit PR's with more upgrades and adjust this one to be the last.  ^.^",
      ClickMath(mathMod(0.0, 10.0)),
    ),
  },
)

let init = () => (
  {
    startTime: 0.0,
    curTime: 0.0,
    lastUpdated: 0.0,
    credits: 0.0,
    clickWorthMath: init_mathMod,
    upgradesRemaining: init_upgrades,
    upgradesUsed: 0,
    messages: list{},
    autoClicker: empty_mathMod,
  },
  Cmd.none,
)

/* Math helpers */
let calc_mathMod_immediate = (initial, math) => (initial +. math.baseAdd) *. math.mult

let calc_mathMod_combine = (math1, math2) => {
  baseAdd: math1.baseAdd +. math2.baseAdd,
  mult: math1.mult *. math2.mult,
}

let worthString = worth => Js.Float.toString(worth)

/* Base definitions */

let baseWorth_click = 0.000

/* Updating functionality */

let calcWorth_click = math => calc_mathMod_immediate(baseWorth_click, math)

let applyUpgrade = (name, model) => {
  let rec aux = (model, untouched, x) =>
    switch x {
    | list{} => {...model, upgradesRemaining: List.rev(untouched)}
    | list{upgrade, ...rest} if upgrade.name != name =>
      aux(model, list{upgrade, ...untouched}, rest)
    | list{upgrade, ..._rest} if upgrade.cost > model.credits => model
    | list{upgrade, ...rest} =>
      /* let () = Js.log ("Upgrading", upgrade) in */
      let newModel = switch upgrade.math {
      | ClickMath(math) => {
          ...model,
          clickWorthMath: calc_mathMod_combine(model.clickWorthMath, math),
          credits: model.credits -. upgrade.cost,
          upgradesUsed: model.upgradesUsed + 1,
          messages: list{
            "Bought " ++ (upgrade.name ++ (" for " ++ (worthString(upgrade.cost) ++ " credits"))),
            ...model.messages,
          },
        }
      | AddAutoClicker(math) => {
          ...model,
          autoClicker: calc_mathMod_combine(model.autoClicker, math),
          credits: model.credits -. upgrade.cost,
          upgradesUsed: model.upgradesUsed + 1,
          messages: list{
            "Bought " ++ (upgrade.name ++ (" for " ++ (worthString(upgrade.cost) ++ " credits"))),
            ...model.messages,
          },
        }
      }
      {
        ...newModel,
        upgradesRemaining: List.append(List.rev(untouched), rest),
      }
    }
  aux(model, list{}, model.upgradesRemaining)
}

let update = model => {
  /* let () = Js.log ("Update", model) in */
  open AnimationFrame

  x =>
    switch x {
    | OnFrame(ev) => (
        {
          ...model,
          startTime: if model.startTime < 1.0 {
            ev.time
          } else {
            model.startTime
          },
          curTime: ev.time,
          credits: model.credits +.
          ev.delta *.
          0.001 *.
          calcWorth_click(model.clickWorthMath) *.
          model.autoClicker.mult *.
          model.autoClicker.baseAdd,
        },
        Cmd.none,
      )
    | Click => (
        {
          ...model,
          credits: model.credits +. calcWorth_click(model.clickWorthMath),
        },
        Cmd.none,
      )
    | DoUpgrade(
        name,
      ) => /* let () = Js.log ("DoUpgrade", name, model, applyUpgrade name model) in */
      (applyUpgrade(name, model), Cmd.none)
    }
}

let subscriptions = model =>
  /* let () = Js.log ("Subscriptions", model) in */
  if model.autoClicker.baseAdd > 0.0 || model.startTime < 1.0 {
    AnimationFrame.every(ev => OnFrame(ev))
  } else {
    Sub.none
  }

/* View handling */

let worthStringText = worth => text(worthString(worth))

let css_topContainer = styles(list{
  ("background-color", "rgb(0,0,0)"),
  ("color", "rgb(255,255,255)"),
  ("vertical-align", "top"),
  ("height", "100%"),
  ("width", "100%"),
})

let styles_container = list{
  ("background-color", "rgb(32,16,16)"),
  ("color", "rgb(212,212,192)"),
  ("vertical-align", "top"),
  /* ; "height", "100%" */
  /* ; "padding", "4px" */
}

let css_container_top = styles(list{("width", "100%"), ...styles_container})

let css_container_bot = styles(list{("width", "100%"), ...styles_container})

let css_container_center = styles(list{("width", "100%"), ...styles_container})

let view_topBar = model =>
  tr(list{css_container_top}, list{td(list{}, list{worthStringText(model.credits)})})

let view_botBar = model =>
  tr(
    list{css_container_bot},
    list{
      td(
        list{},
        List.map(message => div(~key=message, list{}, list{text(message)}), model.messages),
      ),
    },
  )

let rec view_upgrades = (model, x) =>
  switch x {
  | list{upgrade, ...rest} if model.credits >= upgrade.cost => list{
      button(~key=upgrade.name, list{onClick(DoUpgrade(upgrade.name))}, list{text(upgrade.name)}),
      ...view_upgrades(model, rest),
    }
  | _ => list{}
  }

let view_center = model =>
  tr(
    list{css_container_center},
    list{
      td(
        list{},
        list{
          button(list{onClick(Click)}, list{text("Click")}),
          ...view_upgrades(model, model.upgradesRemaining),
        },
      ),
    },
  )

let view = model =>
  table(
    list{css_topContainer},
    list{tbody(list{}, list{view_topBar(model), view_center(model), view_botBar(model)})},
  )

let main = standardProgram({
  init: init,
  update: update,
  view: view,
  subscriptions: subscriptions,
})
