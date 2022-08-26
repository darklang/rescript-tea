type rec t = Js_json.t

type rec kind<'msg> = Js_json.kind<'msg> =
  | String: kind<Js_string.t>
  | Number: kind<float>
  | Object: kind<Js_dict.t<t>>
  | Array: kind<array<t>>
  | Boolean: kind<bool>
  | Null: kind<Js_types.null_val>

type rec tagged_t = Js_json.tagged_t =
  | JSONFalse
  | JSONTrue
  | JSONNull
  | JSONString(string)
  | JSONNumber(float)
  | JSONObject(Js_dict.t<t>)
  | JSONArray(array<t>)

let classify: t => tagged_t

let test: ('msg, kind<'b>) => bool

let decodeString: t => option<Js_string.t>

let decodeNumber: t => option<float>

let decodeObject: t => option<Js_dict.t<t>>

let decodeArray: t => option<array<t>>

let decodeBoolean: t => option<bool>

let decodeNull: t => option<Js_null.t<'msg>>

external string: string => t = "%identity"
external number: float => t = "%identity"
external boolean: bool => t = "%identity"
external object_: Js_dict.t<t> => t = "%identity"
external array: array<t> => t = "%identity"
external stringArray: array<string> => t = "%identity"
external numberArray: array<float> => t = "%identity"
external booleanArray: array<bool> => t = "%identity"
external objectArray: array<Js_dict.t<t>> => t = "%identity"



let deserializeUnsafe: string => 'msg

let serializeExn: 'msg => string

type nothingYet

@val external stringify: ('t, Js.null<nothingYet>, int) => string = "JSON.stringify"

let string_of_json: (~indent: int=?, Js.Undefined.t<'msg>) => string

let of_type: (kind<'msg>, 'msg) => t

let null: Js_types.null_val
