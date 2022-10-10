let at: (list<string>, JsonCombinators.Json.Decode.t<'a>) => JsonCombinators.Json.Decode.t<'a> 

let decodeString: (JsonCombinators.Json.Decode.t<'a>, string) => result<'a, string>

let succeed: 'a => JsonCombinators.Json.Decode.t<'a>

let decodeEvent: (JsonCombinators.Json.Decode.t<'a>, Web_node.event) => result<'a, string>
