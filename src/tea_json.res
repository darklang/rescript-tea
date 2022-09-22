module Decoder = {
  type error = String.t

  module ObjectDict = Belt.Map.String

  type t<'input, 'result> = Decoder('input => result<'result, error>)

  exception ParseFail(string)

  /* Primitive types */

  let int = Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONNumber(n) =>
        if n > float_of_int(min_int) && n < float_of_int(max_int) {
          Ok(int_of_float(n))
        } else {
          Error("number out of int range")
        }
      | _ => Error("Non-int value")
      }
    },
  )

  /* Compound types */

  let array = (Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONArray(a) =>
        let parse = v =>
          switch decoder(v) {
          | Ok(r) => r
          | Error(e) => raise(ParseFail(e))
          }
        try Ok(Array.map(parse, a)) catch {
        | ParseFail(e) => Error("array -> " ++ e)
        }
      | _ => Error("Non-array value")
      }
    },
  )

  let field = (key, Decoder(decoder)) => Decoder(
    value => {
      open Web.Json
      switch classify(value) {
      | JSONObject(o) =>
        switch Js.Dict.get(o, key) {
        | None => Error("Field Value is undefined: " ++ key)
        | Some(v) =>
          switch decoder(v) {
          | Ok(_) as o => o
          | Error(e) => Error("field `" ++ (key ++ ("` -> " ++ e)))
          }
        }
      | _ => Error("Non-fieldable value")
      }
    },
  )

  let map = (mapper, Decoder(decoder1)) => Decoder(
    value => {
      switch decoder1(value) {
      | Ok(v1) => Ok(mapper(v1))
      | Error(e) => Error("map " ++ e)
      }
    },
  )

let map2 = (mapper, Decoder(decoder1), Decoder(decoder2)) => Decoder(
    value => {
      switch (decoder1(value), decoder2(value)) {
      | (Ok(v1), Ok(v2)) => Ok(mapper(v1, v2))
      | (e1, e2) => 
       let result = switch e2 {
        | Error(e) => Some(e)
        | Ok(_) =>
          switch e1 { 
          | Ok(_) => None 
          | Error(e) => Some(e)}}
        switch result {
        | None => failwith("Impossible case")
        | Some(e) => Error("map2 -> " ++ e)
        }
      }
    },
  )



 /* Fancy Primitives */

  let succeed = v => Decoder(_value => Ok(v))


  /* Decoders */

  /* TODO:  Constrain this value type more */
  let decodeValue = (Decoder(decoder), value) =>
    try decoder(value) catch {
    | ParseFail(e) => Error(e)
    | _ => Error("Unknown JSON parsing error")
    }

  let decodeEvent = (Decoder(decoder), value: Web_node.event) =>
    try decoder(Obj.magic(value)) catch {
    | ParseFail(e) => Error(e)
    | _ => Error("Unknown JSON parsing error")
    }

  let decodeString = (decoder, string) =>
    try {
      let value = Web.Json.parseExn(string)
      decodeValue(decoder, value)
    } catch {
    /* | JsException e -> Error ("Given an invalid JSON: " ^ e) */
    | _ => Error("Invalid JSON string")
    }
}

type t = Web.Json.t
