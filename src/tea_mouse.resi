type position = {x: int, y: int}

let position: Tea_json.Decoder.t<Web.Json.t, position>

let registerGlobal: (string, string, position => 'msg) => Tea_sub.t<'msg>

let clicks: (~key: string=?, position => 'msg) => Tea_sub.t<'msg>

let moves: (~key: string=?, position => 'msg) => Tea_sub.t<'msg>

let downs: (~key: string=?, position => 'msg) => Tea_sub.t<'msg>

let ups: (~key: string=?, position => 'msg) => Tea_sub.t<'msg>

