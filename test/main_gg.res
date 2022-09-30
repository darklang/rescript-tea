open Tea

module IntMap = Map.Make({
  type t = int
  let compare = compare
})

module IntPairMap = Map.Make({
  type t = (int, int)
  let compare = compare
})

let maxInfluence = 255

type objectTypes =
  /* Base (team, influenceAmt) */
  | Base(int, int)

type model = {
  map: array<array<int>>,
  influence: array<array<int>>,
  units: IntPairMap.t<list<objectTypes>>,
  pTeamResources: int,
  nTeamResources: int,
}

type msg =
  | OnUrlChange(array<array<int>>)
  | AITick
  | InfluenceTick

let toUrl = _model => ""

open Rescript_json_combinators_extended
let fromUrl = url => {
  let mapDecoder = {
    JsonCombinators.Json.Decode.array(JsonCombinators.Json.Decode.array(JsonCombinators.Json.Decode.int))
  }
  switch decodeString(mapDecoder, url) {
  | Ok(map) => map
  | Error(_) => Array.make_matrix(32, 32, 0)
  }
}

let init = ((), location) => {
  open Web.Location
  let map = fromUrl(location.hash)
  let width = Array.length(map)
  let height = Array.length(map[0])
  let units =
    IntPairMap.empty
    |> IntPairMap.add((4, height - 4), list{Base(1, 12)})
    |> IntPairMap.add((width - 4, 4), list{Base(-1, 12)})

  let model = {
    map: map,
    influence: Array.make_matrix(width, height, 0),
    units: units,
    pTeamResources: 1000,
    nTeamResources: 1000,
  }
  (model, Cmd.none)
}

let addUnit = (coords, unit, dict) => {
  let list = if IntPairMap.mem(coords, dict) {
    IntPairMap.find(coords, dict)
  } else {
    list{}
  }
  IntPairMap.add(coords, list{unit, ...list}, dict)
}

let rec mergeUnits = (dict, x) =>
  switch x {
  | list{} => dict
  | list{(coords, unit), ...rest} => addUnit(coords, unit, mergeUnits(dict, rest))
  }

let update = (model, x) =>
  switch x {
  | OnUrlChange(_map) => (model, Cmd.none)
  | AITick =>
    let width = Array.length(model.map)
    let height = Array.length(model.map[0])
    let influence = model.influence
    let units = model.units
    let tickUnit = ((x, y): (int, int), ret, Base(team, inf)) =>{

        /* Add in test to use up resources when building and to destroy self if not enough influence here */
        let newBase = Base(team, inf)
        if 
          (x > 1) &&
            (y > 1) &&
            (influence[x - 1][y - 1] * team > 2) && (false == IntPairMap.mem((x - 1, y - 1), units))
         {
          list{((x - 1, y - 1), newBase), ...ret}
        } else if (
          x > 1 &&
            (y < height - 2 &&
            (influence[x - 1][y + 1] * team > 2 && false == IntPairMap.mem((x - 1, y + 1), units)))
        ) {
          list{((x - 1, y + 1), newBase), ...ret}
        } else if (
          x < width - 2 &&
            (y > 1 &&
            (influence[x + 1][y - 1] * team > 2 && false == IntPairMap.mem((x + 1, y - 1), units)))
        ) {
          list{((x + 1, y - 1), newBase), ...ret}
        } else if (
          x < width - 2 &&
            (y < height - 2 &&
            (influence[x + 1][y + 1] * team > 2 && false == IntPairMap.mem((x + 1, y + 1), units)))
        ) {
          list{((x + 1, y + 1), newBase), ...ret}
        } else {
          ret
        }}
      
    let tickUnits = (coords, units, ret) => List.fold_left(tickUnit(coords), ret, units)
    let newUnits = IntPairMap.fold(tickUnits, units, list{})
    let units = mergeUnits(units, newUnits)
    (
      {
        ...model,
        units: units,
      },
      Cmd.none,
    )
  | InfluenceTick =>
    let width = Array.length(model.map)
    let height = Array.length(model.map[0])
    let influence = Array.copy(model.influence)
    let rec addInfluence = ((x, y), rest) =>
      switch rest {
      | list{} => ()
      | list{Base(team, amt), ...rest} =>
        let () = influence[x][y] = model.influence[x][y] + amt * team
        addInfluence((x, y), rest)
      }
    /* | _ :: rest -> addInfluence (x, y) rest */
    let () = IntPairMap.iter(addInfluence, model.units)
    let processInfY = (x, y, t) =>
      /* Process this out to changes to read */
      if x == 0 || (x == width - 1 || (y == 0 || y == height - 1)) {
        influence[x][y] = 0
      } else if t == 0 {
        let inf = model.influence[x][y]
        let infXN = model.influence[x - 1][y]
        let infXP = model.influence[x + 1][y]
        let infYN = model.influence[x][y - 1]
        let infYP = model.influence[x][y + 1]
        let part = inf / 6
        /* let comp = inf - part in */
        let () = /* if infXN < comp then */ influence[x - 1][y] = infXN + part
        let () = /* if infXP < comp then */ influence[x + 1][y] = infXP + part
        let () = /* if infYN < comp then */ influence[x][y - 1] = infYN + part
        let () = /* if infYP < comp then */ influence[x][y + 1] = infYP + part
        influence[x][y] = inf - part * 4
      } else {
        ()
      }
    let processX = (x, yArr) => Array.iteri(processInfY(x), yArr)
    let reduceInfY = (x, y, i) =>
      if i < -255 {
        influence[x][y] = -255
      } else if i < 0 {
        influence[x][y] = i + 1
      } else if i > 255 {
        influence[x][y] = 255
      } else if i > 0 {
        influence[x][y] = i - 1
      } else {
        ()
      }
    let reduceInfX = (x, yArr) => Array.iteri(reduceInfY(x), yArr)
    let () = Array.iteri(processX, model.map)
    let () = Array.iteri(reduceInfX, influence)
    let (scoreN, scoreP) = {
      let scoreY = ((sn, sp), inf) =>
        if inf < -7 {
          (sn - inf + 7, sp)
        } else if inf > 7 {
          (sn, sp + inf - 7)
        } else {
          (sn, sp)
        }
      let scoreX = (scores, yArr) => Array.fold_left(scoreY, scores, yArr)
      Array.fold_left(scoreX, (0, 0), influence)
    }
    let nTeamResources = model.nTeamResources + scoreN
    let pTeamResources = model.pTeamResources + scoreP
    (
      {
        ...model,
        influence: influence,
        nTeamResources: nTeamResources,
        pTeamResources: pTeamResources,
      },
      Cmd.none,
    )
  }

let subscriptions = _model =>
  Sub.batch(list{
    Time.every(~key="",Time.inMilliseconds(500.0), _ => InfluenceTick),
    Time.every(~key="",Time.inMilliseconds(1100.0), _ => AITick),
  })

let cellSize = 12 /* Always be an even integral */
let cellSizeStr = string_of_int(cellSize)
let cellSizeHalf = cellSize / 2
let cellSizeHalfStr = string_of_int(cellSizeHalf)

let view = model => {
  open Svg
  open Svg.Attributes
  let mapWidth = Array.length(model.map)
  let mapHeight = Array.length(model.map[0])
  let unitsSvg = () => {
    let rec unitToSvg = ((coords, units)) =>
      switch units {
      | list{} => list{}
      | list{Base(team, _influence), ...rest} => list{
          circle(
            list{
              r(cellSizeHalfStr),
              fill(
                if team < 0 {
                  "#FF0000"
                } else if team > 0 {
                  "#0000FF"
                } else {
                  "#111111"
                },
              ),
              stroke("#000000"),
              cx(string_of_int(fst(coords) * cellSize + cellSizeHalf)),
              cy(string_of_int(snd(coords) * cellSize + cellSizeHalf)),
            },
            list{},
          ),
          ...unitToSvg((coords, rest)),
        }
      }
    IntPairMap.bindings(model.units) |> List.map(unitToSvg) |> List.flatten
  }
  let tileToSvg = (cx, cy, value) => {
    let key = string_of_int(value)
    if cx == 0 || (cx == mapWidth - 1 || (cy == 0 || cy == mapHeight - 1)) {
      noNode
    } else if value == 0 {
      /* Open */
      let v = model.influence[cx][cy]
      let fillColor = {
        let av = abs(v)
        let density = if av >= maxInfluence {
          "0"
        } else {
          string_of_int(255 - av)
        }
        let color = if v < 0 {
          "rgb(255," ++ (density ++ ("," ++ (density ++ ")")))
        } else if v > 0 {
          "rgb(" ++ (density ++ ("," ++ (density ++ ",255)")))
        } else {
          "rgb(255,255,255)"
        }
        color
      }
      rect(
        list{
          x(string_of_int(cx * cellSize)),
          y(string_of_int(cy * cellSize)),
          width(cellSizeStr),
          height(cellSizeStr),
          fill(fillColor),
          stroke(
            if v < 0 {
              "#FF0000"
            } else if v > 0 {
              "#0000FF"
            } else {
              "#FFFFFF"
            },
          ),
        },
        list{},
      )
    } else if value == 1 {
      /* Wall */
      rect(
        ~key,
        list{
          x(string_of_int(cx * cellSize)),
          y(string_of_int(cy * cellSize)),
          width(cellSizeStr),
          height(cellSizeStr),
          fill("#222222"),
          stroke("#000000"),
        },
        list{},
      )
    } else {
      rect(
        ~key,
        list{
          x(string_of_int(cx * cellSize)),
          y(string_of_int(cy * cellSize)),
          width(cellSizeStr),
          height(cellSizeStr),
          fill("#FF88FF"),
          stroke("#FF00FF"),
        },
        list{},
      )
    }
  }

  let tileMapperX = (cx, value) => {
    let values = Array.mapi(tileToSvg(cx), value)
    Array.to_list(values)
  }
  let valueArray = Array.mapi(tileMapperX, model.map)
  Html.div(
    list{},
    list{
      svg(
        list{
          viewBox(
            "0 0 " ++
            (string_of_int(mapWidth * cellSize) ++
            (" " ++ string_of_int(mapHeight * cellSize))),
          ),
          width(string_of_int(mapWidth * cellSize) ++ "px"),
        },
        list{
          g(list{}, Array.to_list(valueArray) |> List.concat),
          lazy1(string_of_int(IntPairMap.cardinal(model.units)), () => g(list{}, unitsSvg())),
        },
      ),
    },
  )
}

let locationToMessage = location => {
  open Web.Location
  OnUrlChange(fromUrl(location.hash))
}

let main = {
  open Navigation
  navigationProgram(
    locationToMessage,
    {
      init: init,
      update: update,
      view: view,
      subscriptions: subscriptions,
      shutdown: _model => Cmd.none,
    },
  )
}
