type t = {
  "hash": string,
  "hash#=": Js_OO.Meth.arity1<string => unit>,
  "host": string,
  "host#=": Js_OO.Meth.arity1<string => unit>,
  "hostname": string,
  "hostname#=": Js_OO.Meth.arity1<string => unit>,
  "href": string,
  "href#=": Js_OO.Meth.arity1<string => unit>,
  "origin": string,
  "password": string,
  "password#=": Js_OO.Meth.arity1<string => unit>,
  "pathname": string,
  "pathname#=": Js_OO.Meth.arity1<string => unit>,
  "port": string,
  "port#=": Js_OO.Meth.arity1<string => unit>,
  "protocol": string,
  "protocol#=": Js_OO.Meth.arity1<string => unit>,
  "search": string,
  "search#=": Js_OO.Meth.arity1<string => unit>,
  "username": string,
  "username#=": Js_OO.Meth.arity1<string => unit>,
}

let getHref: {.."href": 'msg} => 'msg

let setHref: ({.."href#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getProtocol: {.."protocol": 'msg} => 'msg

let setProtocol: ({.."protocol#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getHost: {.."host": 'msg} => 'msg

let setHost: ({.."host#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getHostname: {.."hostname": 'msg} => 'msg

let setHostname: ({.."hostname#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getPort: {.."port": 'msg} => 'msg

let setPort: ({.."port#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getPathname: {.."pathname": 'msg} => 'msg

let setPathname: ({.."pathname#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getSearch: {.."search": 'msg} => 'msg

let setSearch: ({.."search#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getHash: {.."hash": 'msg} => 'msg

let setHash: ({.."hash#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getUsername: {.."username": 'msg} => 'msg

let setUsername: ({.."username#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getPassword: {.."password": 'msg} => 'msg

let setPassword: ({.."password#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let getOrigin: {.."origin": 'msg} => 'msg

type location = {
  href: string,
  protocol: string,
  host: string,
  hostname: string,
  port: string,
  pathname: string,
  search: string,
  hash: string,
  username: string,
  password: string,
  origin: string,
}

let asRecord: {..
  "hash": string,
  "host": string,
  "hostname": string,
  "href": string,
  "origin": string,
  "password": string,
  "pathname": string,
  "port": string,
  "protocol": string,
  "search": string,
  "username": string,
} => location
