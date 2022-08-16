module Decoder = {
  type error = String.t

  module ObjectDict = Map.Make(String)

  type t<'input, 'result> = Decoder('input => Belt.Result.t<'result, error>)
  /* 
    | Parser : (Web.Json.t -> ('result, error) Belt.Result.t) -> ('result, error) Belt.Result.t t
 */
  /* 
    | Value : (Web.Json.t, error) Belt.Result.t t
    | Succeed : 'result -> ('result, error) Belt.Result.t t
    | Fail : error -> (_, error) Belt.Result.t t
    | Null : 'result -> ('result, error) Belt.Result.t t
    | String : (string, error) Belt.Result.t t
 */

  exception ParseFail(string)

  /* Primitive types */

  let string = Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONString(s) => Belt.Result.Ok(s)
      | _ => Belt.Result.Error("Non-string value")
      }
    },
  )

  let int = Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONNumber(n) =>
        if n > float_of_int(min_int) && n < float_of_int(max_int) {
          Belt.Result.Ok(int_of_float(n))
        } else {
          Belt.Result.Error("number out of int range")
        }
      | _ => Belt.Result.Error("Non-int value")
      }
    },
  )

  let float = Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONNumber(n) => Belt.Result.Ok(n)
      | _ => Belt.Result.Error("Non-float-value")
      }
    },
  )

  let bool = Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONTrue => Belt.Result.Ok(true)
      | JSONFalse => Belt.Result.Ok(false)
      | _ => Belt.Result.Error("Non-boolean value")
      }
    },
  )

  let null = v => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONNull => Belt.Result.Ok(v)
      | _ => Belt.Result.Error("Non-null value")
      }
    },
  )

  /* Compound types */

  let list = (Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONArray(a) =>
        let parse = v =>
          switch decoder(v) {
          | Belt.Result.Ok(r) => r
          | Belt.Result.Error(e) => raise(ParseFail(e))
          }
        try Belt.Result.Ok(Array.to_list(a) |> List.map(parse)) catch {
        | ParseFail(e) => Belt.Result.Error("list -> " ++ e)
        }
      | _ => Belt.Result.Error("Non-list value")
      }
    },
  )

  let array = (Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONArray(a) =>
        let parse = v =>
          switch decoder(v) {
          | Belt.Result.Ok(r) => r
          | Belt.Result.Error(e) => raise(ParseFail(e))
          }
        try Belt.Result.Ok(Array.map(parse, a)) catch {
        | ParseFail(e) => Belt.Result.Error("array -> " ++ e)
        }
      | _ => Belt.Result.Error("Non-array value")
      }
    },
  )

  let keyValuePairs = (Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONObject(o) =>
        let keys = Js.Dict.keys(o)
        let parse = (k, l) =>
          switch Js.Dict.get(o, k) {
          | None => raise(ParseFail("Key is undefined: " ++ k))
          | Some(v) =>
            switch decoder(v) {
            | Belt.Result.Ok(r) => list{(k, r), ...l}
            | Belt.Result.Error(e) => raise(ParseFail(e))
            }
          }
        try Belt.Result.Ok(Array.fold_right(parse, keys, list{})) catch {
        | ParseFail(e) => Belt.Result.Error("Invalid keyValuePair parsing: " ++ e)
        }
      | _ => Belt.Result.Error("Non-keyValuePair value")
      }
    },
  )

  let dict = (Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONObject(o) =>
        let keys = Js.Dict.keys(o)
        let parse = (k, d) =>
          switch Js.Dict.get(o, k) {
          | None => raise(ParseFail("Key is undefined: " ++ k))
          | Some(v) =>
            switch decoder(v) {
            | Belt.Result.Ok(r) => ObjectDict.add(k, r, d)
            | Belt.Result.Error(e) => raise(ParseFail(e))
            }
          }
        let emptyDict = ObjectDict.empty
        try Belt.Result.Ok(Array.fold_right(parse, keys, emptyDict)) catch {
        | ParseFail(e) => Belt.Result.Error("Invalid dict parsing: " ++ e)
        }
      | _ => Belt.Result.Error("Non-dict value")
      }
    },
  )

  let field = (key, Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONObject(o) =>
        switch Js.Dict.get(o, key) {
        | None => Belt.Result.Error("Field Value is undefined: " ++ key)
        | Some(v) =>
          switch decoder(v) {
          | Ok(_) as o => o
          | Error(e) => Error("field `" ++ (key ++ ("` -> " ++ e)))
          }
        }
      | _ => Belt.Result.Error("Non-fieldable value")
      }
    },
  )

  let at = (fields, dec) => List.fold_right(field, fields, dec)

  let index = (idx, Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONArray(a) =>
        if idx < 0 || idx > Array.length(a) {
          Belt.Result.Error("Array index out of range: " ++ string_of_int(idx))
        } else {
          decoder(a[idx])
        }
      | _ => Belt.Result.Error("Non-array value")
      }
    },
  )

  let maybe = (Decoder(decoder)) => Decoder(
    value =>
      switch decoder(value) {
      | Belt.Result.Ok(r) => Belt.Result.Ok(Some(r))
      | Belt.Result.Error(_) => Belt.Result.Ok(None)
      },
  )

  let oneOf = decoders => Decoder(
    value => {
      let rec parse = (v, x) =>
        switch x {
        | list{} => Belt.Result.Error("No one-of's matched")
        | list{Decoder(decoder), ...rest} =>
          try switch decoder(v) {
          | Belt.Result.Ok(_) as ok => ok
          | Belt.Result.Error(_) => parse(v, rest)
          } catch {
          | _ => parse(v, rest)
          }
        }
      parse(value, decoders)
    },
  )

  let map = (mapper, Decoder(decoder1)) => Decoder(
    value => {
      open Tea_result
      switch decoder1(value) {
      | Ok(v1) => Ok(mapper(v1))
      | Error(e) => Error("map " ++ e)
      }
    },
  )

  let map2 = (mapper, Decoder(decoder1), Decoder(decoder2)) => Decoder(
    value => {
      open Tea_result
      switch (decoder1(value), decoder2(value)) {
      | (Ok(v1), Ok(v2)) => Ok(mapper(v1, v2))
      | (e1, e2) =>
        switch Tea_result.error_of_first(e1, e2) {
        | None => failwith("Impossible case")
        | Some(e) => Error("map2 -> " ++ e)
        }
      }
    },
  )

  let map3 = (mapper, Decoder(decoder1), Decoder(decoder2), Decoder(decoder3)) => Decoder(
    value => {
      open Tea_result
      switch (decoder1(value), decoder2(value), decoder3(value)) {
      | (Ok(v1), Ok(v2), Ok(v3)) => Ok(mapper(v1, v2, v3))
      | (e1, e2, e3) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map3 -> " ++ e)
        }
      }
    },
  )

  let map4 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
  ) => Decoder(
    value => {
      open Tea_result
      switch (decoder1(value), decoder2(value), decoder3(value), decoder4(value)) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4)) => Ok(mapper(v1, v2, v3, v4))
      | (e1, e2, e3, e4) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map4 -> " ++ e)
        }
      }
    },
  )

  let map5 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
  ) => Decoder(
    value => {
      open Tea_result
      switch (decoder1(value), decoder2(value), decoder3(value), decoder4(value), decoder5(value)) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5)) => Ok(mapper(v1, v2, v3, v4, v5))
      | (e1, e2, e3, e4, e5) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) |> first(e5) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map5 -> " ++ e)
        }
      }
    },
  )

  let map6 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
    Decoder(decoder6),
  ) => Decoder(
    value => {
      open Tea_result
      switch (
        decoder1(value),
        decoder2(value),
        decoder3(value),
        decoder4(value),
        decoder5(value),
        decoder6(value),
      ) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5), Ok(v6)) => Ok(mapper(v1, v2, v3, v4, v5, v6))
      | (e1, e2, e3, e4, e5, e6) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) |> first(e5) |> first(e6) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map6 -> " ++ e)
        }
      }
    },
  )

  let map7 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
    Decoder(decoder6),
    Decoder(decoder7),
  ) => Decoder(
    value => {
      open Tea_result
      switch (
        decoder1(value),
        decoder2(value),
        decoder3(value),
        decoder4(value),
        decoder5(value),
        decoder6(value),
        decoder7(value),
      ) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5), Ok(v6), Ok(v7)) =>
        Ok(mapper(v1, v2, v3, v4, v5, v6, v7))
      | (e1, e2, e3, e4, e5, e6, e7) =>
        open! Tea_result
        switch e1 |> first(e2) |> first(e3) |> first(e4) |> first(e5) |> first(e6) |> first(e7) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map7 -> " ++ e)
        }
      }
    },
  )

  let map8 = (
    mapper,
    Decoder(decoder1),
    Decoder(decoder2),
    Decoder(decoder3),
    Decoder(decoder4),
    Decoder(decoder5),
    Decoder(decoder6),
    Decoder(decoder7),
    Decoder(decoder8),
  ) => Decoder(
    value => {
      open Tea_result
      switch (
        decoder1(value),
        decoder2(value),
        decoder3(value),
        decoder4(value),
        decoder5(value),
        decoder6(value),
        decoder7(value),
        decoder8(value),
      ) {
      | (Ok(v1), Ok(v2), Ok(v3), Ok(v4), Ok(v5), Ok(v6), Ok(v7), Ok(v8)) =>
        Ok(mapper(v1, v2, v3, v4, v5, v6, v7, v8))
      | (e1, e2, e3, e4, e5, e6, e7, e8) =>
        open! Tea_result
        switch e1
        |> first(e2)
        |> first(e3)
        |> first(e4)
        |> first(e5)
        |> first(e6)
        |> first(e7)
        |> first(e8) {
        | Ok(_) => failwith("Impossible case")
        | Error(e) => Error("map8 -> " ++ e)
        }
      }
    },
  )

  /* Fancy Primitives */

  let succeed = v => Decoder(_value => Belt.Result.Ok(v))

  let fail = e => Decoder(_value => Belt.Result.Error(e))

  let value = Decoder(value => Belt.Result.Ok(value))

  let andThen = (func, Decoder(decoder)) => Decoder(
    value =>
      switch decoder(value) {
      | Belt.Result.Ok(r) =>
        let Decoder(andThenDecoder) = func(r)
        andThenDecoder(value)
      | Belt.Result.Error(_) as err => err
      },
  )

  let lazy_ = func => andThen(func, succeed())

  let nullable = decoder => oneOf(list{null(None), map(v => Some(v), decoder)})

  /* Decoders */

  /* TODO:  Constrain this value type more */
  let decodeValue = (Decoder(decoder), value) =>
    try decoder(value) catch {
    | ParseFail(e) => Belt.Result.Error(e)
    | _ => Belt.Result.Error("Unknown JSON parsing error")
    }

  let decodeEvent = (Decoder(decoder), value: Web_node.event) =>
    try decoder(Obj.magic(value)) catch {
    | ParseFail(e) => Belt.Result.Error(e)
    | _ => Belt.Result.Error("Unknown JSON parsing error")
    }

  let decodeString = (decoder, string) =>
    try {
      let value = Web.Json.parseExn(string)
      decodeValue(decoder, value)
    } catch {
    /* | JsException e -> Belt.Result.Error ("Given an invalid JSON: " ^ e) */
    | _ => Belt.Result.Error("Invalid JSON string")
    }
}

module Encoder = {
  open Web

  type t = Json.t

  let encode = (indentLevel, value) =>
    Web.Json.string_of_json(~indent=indentLevel, Js.Undefined.return(value))

  /* Encoders */

  let string = (v: string) => Json.of_type(Json.String, v)

  let int = (v: int) => Json.of_type(Json.Number, float_of_int(v))

  let float = (v: float) => Json.of_type(Json.Number, v)

  let bool = (v: bool) => Json.of_type(Json.Boolean, v)

  let null = Json.of_type(Json.Null, Json.null)

  let object_ = v => {
    let aux = (o, (k, v)) => {
      let () = Js.Dict.set(o, k, v)
      o
    }
    let o = List.fold_left(aux, Js.Dict.empty(), v)
    Json.of_type(Json.Object, o)
  }

  let array = (v: array<'t>) => Json.of_type(Json.Array, v)

  let list = (v: list<'t>) => Json.of_type(Json.Array, Array.of_list(v))
}

type t = Web.Json.t
