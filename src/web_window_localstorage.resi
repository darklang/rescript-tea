type t = {"length": int}

let length: {.."localStorage": Js.Undefined.t<{.."length": 'msg}>} => option<'msg>

let clear: {.."localStorage": Js.Undefined.t<t>} => option<unit>

let key: ({.."localStorage": Js.Undefined.t<t>}, int) => option<string>

let getItem: ({.."localStorage": Js.Undefined.t<t>}, string) => option<string>

let removeItem: ({.."localStorage": Js.Undefined.t<t>}, string) => option<unit>

let setItem: ({.."localStorage": Js.Undefined.t<t>}, string, string) => option<unit>
