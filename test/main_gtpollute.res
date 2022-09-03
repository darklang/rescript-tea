open Tea

/* This is a direct conversion from other code, not made to be pretty or to demonstrate how things should be done */
/* I.E.  Ignore this. */

type model = {
  data: array<array<int>>,
  maxValue: int,
  value: int,
  tickTime: float,
}

type msg =
  | UpdateValue(string)
  | UpdateTickTime(string)
  | DoUpdate(Time.t)

let init = () => (
  {
    data: Array.make_matrix(31, 31, 0),
    maxValue: 1,
    value: 0,
    tickTime: 5.0,
  },
  Cmd.none,
)

let update = (model, x) =>
  switch x {
  | UpdateValue(sValue) =>
    let value = try int_of_string(sValue) catch {
    | _ => model.value
    }
    ({...model, value}, Cmd.none)
  | UpdateTickTime(sValue) =>
    let tickTime = try float_of_string(sValue) catch {
    | _ => model.tickTime
    }
    ({...model, tickTime}, Cmd.none)
  | DoUpdate(_) =>
    let data: array<array<int>> = {
      let data = model.data
      let fullX = Array.length(data)
      let fullY = Array.length(data[0])
      let halfX = fullX / 2
      let halfY = fullY / 2
      let () = data[halfX][halfY] = data[halfX][halfY] + model.value
      let () = {
        let () = Array.fill(data[0], 0, fullY, 0)
        let () = Array.fill(data[fullX - 2], 0, fullY, 0)
        for x in 1 to Array.length(data) - 2 {
          let () = data[x][0] = 0
          let () = data[x][fullY - 2] = 0
          for y in 1 to Array.length(data[0]) - 2 {
            let () = data[x][y] = data[x][y] - 3000
            let () = if data[x][y] < 0 {
              data[x][y] = 0
            } else {
              ()
            }
            let processSide = (xx, yy) =>
              if data[xx][yy] * 12 < data[x][y] * 10 {
                let diff = data[x][y] - data[xx][yy]
                let diff = diff / 20
                let () = data[xx][yy] = data[xx][yy] + diff
                let () = data[x][y] = data[x][y] - diff
              } else {
                ()
              }
            let () = processSide(x - 1, y)
            let () = processSide(x + 1, y)
            let () = processSide(x, y - 1)
            let () = processSide(x, y + 1)
          }
        }
      }
      data
    }
    let maxValue = {
      let inner = (value, arr) => Array.fold_left((maxValue, value) =>
          if maxValue > value {
            maxValue
          } else {
            value
          }
        , value, arr)
      Array.fold_left(inner, 1, data)
    }
    ({...model, data, maxValue}, Cmd.none)
  }

let subscriptions = (model: model) => Time.every(~key="",Time.inMilliseconds(model.tickTime), t => DoUpdate(t))

let view = model => {
  open Svg
  open Svg.Attributes
  let mapperY = (coordX, coordY, value) => {
    let colorHex = {
      let red = if value > 2000000 {
        "FF"
      } else if value > 1000000 {
        "40"
      } else {
        "00"
      }
      let green = if value > 750000 {
        "FF"
      } else if value > 550000 {
        "80"
      } else {
        "00"
      }
      let blue = {
        let bVal = value * 255 / model.maxValue
        if bVal > 255 {
          "FF"
        } else if bVal < 0 {
          "00"
        } else {
          `${string_of_int(bVal)}`
        }
      }
      "#" ++ (red ++ (green ++ blue))
    }
    let strokeHex = if value > 0 {
      "#FFFFFF"
    } else {
      "#000000"
    }
    rect(
      list{
        x(string_of_int(coordX * 10)),
        y(string_of_int(coordY * 10)),
        width("10"),
        height("10"),
        fill(colorHex),
        stroke(strokeHex),
      },
      list{},
    )
  }
  let mapperX = (coordX, value) => {
    let values = Array.mapi(mapperY(coordX), value)
    Array.to_list(values)
  }
  let valueArray = Array.mapi(mapperX, model.data)
  let valueStep = string_of_int(model.value / 20)
  /* if model.value <= 100000 then */
  /* "10000" else */
  /* if model.value <= 1000000 then */
  /* "100000" */
  /* else "1000000" in */
  Html.div(
    list{},
    list{
      Html.label(list{}, list{text("Center input value per tick:")}),
      Html.input'(
        list{
          Html.Attributes.placeholder("Integer value"),
          Html.Events.onInput(s => UpdateValue(s)),
          Html.Attributes.type'("number"),
          Html.Attributes.min("0"),
          Html.Attributes.max("100000000"),
          Html.Attributes.step(valueStep),
          Html.Attributes.value(string_of_int(model.value)),
        },
        list{},
      ),
      Html.br(list{}),
      Html.label(list{}, list{text("Time per tick:")}),
      Html.input'(
        list{
          Html.Attributes.placeholder("Time per tick (ms)"),
          Html.Events.onInput(s => UpdateTickTime(s)),
          Html.Attributes.type'("number"),
          Html.Attributes.min("5"),
          Html.Attributes.step("5"),
          Html.Attributes.value(int_of_float(model.tickTime) |> string_of_int),
        },
        list{},
      ),
      Html.br(list{}),
      svg(list{viewBox("0 0 300 300"), width("300px")}, Array.to_list(valueArray) |> List.concat),
    },
  )
}

let main = {
  open App
  standardProgram({
    init,
    update,
    view,
    subscriptions,
  })
}
