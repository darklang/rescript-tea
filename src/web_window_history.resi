type t = {"length": int, "state": Js.Json.t}

let length: {.."history": Js.Undefined.t<{.."length": int}>} => int

let back: {.."history": Js.Undefined.t<t>} => unit

let forward: {.."history": Js.Undefined.t<t>} => unit

let go: ({.."history": Js.Undefined.t<t>}, int) => unit

let pushState: ({.."history": Js.Undefined.t<t>}, Js.Json.t, string, string) => unit

let replaceState: ({.."history": Js.Undefined.t<t>}, Js.Json.t, string, string) => unit

let state: {.."history": Js.Undefined.t<{.."state": Js.Undefined.t<'msg>}>} => Js.Undefined.t<'msg>
