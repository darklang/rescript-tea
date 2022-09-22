module Decoder: {
  type error = String.t
  module ObjectDict: {
    type rec key = String.t
    type rec t<'msg> = Belt.Map.String.t<'msg>
  }

  type t<'input, 'result> = Decoder('input => result<'result, error>)

  type exn +=  ParseFail(string)

  let int: t<Web.Json.t, int>

  let array: t<Web.Json.t, 'msg> => t<Web.Json.t, array<'msg>>

  let field: (Js.Dict.key, t<Web.Json.t, 'msg>) => t<Web.Json.t, 'msg>

  let map: ('msg => 'b, t<'c, 'msg>) => t<'c, 'b>

  let map2: (('msg, 'b) => 'c, t<'d, 'msg>, t<'d, 'b>) => t<'d, 'c>


  let succeed: 'msg => t<'b, 'msg>


  let decodeValue: (t<'msg, 'b>, 'msg) => result<'b, error>

  let decodeEvent: (t<'msg, 'b>, Web_node.event) => result<'b, error>

  let decodeString: (t<Web.Json.t, 'msg>, string) => result<'msg, error>

}

type t = Web.Json.t
