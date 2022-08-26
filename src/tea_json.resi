module Decoder: {
  type error = String.t
  module ObjectDict: {
    type rec key = String.t
    type rec t<'msg> = Belt.Map.String.t<'msg>
  }

  type t<'input, 'result> = Decoder('input => result<'result, error>)

  type exn +=  ParseFail(string)

  let string: t<Web.Json.t, string>

  let int: t<Web.Json.t, int>

  let float: t<Web.Json.t, float>

  let bool: t<Web.Json.t, bool>

  let null: 'msg => t<Web.Json.t, 'msg>

  let list: t<Web.Json.t, 'msg> => t<Web.Json.t, list<'msg>>

  let array: t<Web.Json.t, 'msg> => t<Web.Json.t, array<'msg>>

  let keyValuePairs: t<Web.Json.t, 'msg> => t<Web.Json.t, list<(Js.Dict.key, 'msg)>>

  let dict: t<Web.Json.t, 'msg> => t<Web.Json.t, ObjectDict.t<'msg>>

  let field: (Js.Dict.key, t<Web.Json.t, 'msg>) => t<Web.Json.t, 'msg>

  let at: (list<Js.Dict.key>, t<Web.Json.t, 'msg>) => t<Web.Json.t, 'msg>

  let index: (int, t<Web.Json.t, 'msg>) => t<Web.Json.t, 'msg>

  let maybe: t<'msg, 'b> => t<'msg, option<'b>>

  let oneOf: list<t<'msg, 'b>> => t<'msg, 'b>

  let map: ('msg => 'b, t<'c, 'msg>) => t<'c, 'b>

  let map2: (('msg, 'b) => 'c, t<'d, 'msg>, t<'d, 'b>) => t<'d, 'c>

  let map3: (('msg, 'b, 'c) => 'd, t<'e, 'msg>, t<'e, 'b>, t<'e, 'c>) => t<'e, 'd>

  let map4: (('msg, 'b, 'c, 'd) => 'e, t<'f, 'msg>, t<'f, 'b>, t<'f, 'c>, t<'f, 'd>) => t<'f, 'e>

  let map5: (('msg, 'b, 'c, 'd, 'e) => 'f, t<'g, 'msg>, t<'g, 'b>, t<'g, 'c>, t<'g, 'd>, t<'g, 'e>) => t<'g,'f,>
  
  let map6: (('msg, 'b, 'c, 'd, 'e, 'f) => 'g, t<'h, 'msg>, t<'h, 'b>, t<'h, 'c>, t<'h, 'd>, t<'h, 'e>, t<'h, 'f>,) => t<'h, 'g>
  
  let map7: (('msg, 'b, 'c, 'd, 'e, 'f, 'g) => 'h, t<'i, 'msg>, t<'i, 'b>, t<'i, 'c>, t<'i, 'd>, t<'i, 'e>, t<'i, 'f>, t<'i, 'g>,) => t<'i, 'h>
  
  let map8: (
  ('msg, 'b, 'c, 'd, 'e, 'f, 'g, 'h) => 'i, t<'j, 'msg>, t<'j, 'b>, t<'j, 'c>, t<'j, 'd>, t<'j, 'e>, t<'j, 'f>, t<'j, 'g>, t<'j, 'h>,) => t<'j, 'i>



  let succeed: 'msg => t<'b, 'msg>

  let fail: error => t<'msg, 'b>

  let value: t<'msg, 'msg>

  let andThen: ('msg => t<'b, 'c>, t<'b, 'msg>) => t<'b, 'c>

  let lazy_: (unit => t<'msg, 'b>) => t<'msg, 'b>

  let nullable: t<Web.Json.t, 'msg> => t<Web.Json.t, option<'msg>>

  let decodeValue: (t<'msg, 'b>, 'msg) => result<'b, error>

  let decodeEvent: (t<'msg, 'b>, Web_node.event) => result<'b, error>

  let decodeString: (t<Web.Json.t, 'msg>, string) => result<'msg, error>

}


module Encoder: {
  type t = Web.Json.t
  let encode: (int, 'msg) => string
  let string: string => Web.Json.t
  let int: int => Web.Json.t
  let float: float => Web.Json.t
  let bool: bool => Web.Json.t
  let null: Web.Json.t
  let object_: list<(Js.Dict.key, Web.Json.t)> => Web.Json.t
  let array: array<Web.Json.t> => Web.Json.t
  let list: list<Web.Json.t> => Web.Json.t
}


type t = Web.Json.t
