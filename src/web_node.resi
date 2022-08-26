type style = {
  "setProperty": Js.undefined<Web_json.t>,
  "setProperty__": Js_OO.Meth.arity3<(string, Js.null<string>, Js.null<string>) => unit>,
}

@send external
setProperty__: (string, Js.null<string>, Js.null<string>) => unit="setProperty__"

type rec t = {
  "checked": Js.undefined<bool>,
  "checked#=": Js_OO.Meth.arity1<Js.undefined<bool> => unit>,
  "childNodes": Js.Array.t<t>,
  "firstChild": Js.Null.t<t>,
  "nodeValue": Js.null<string>,
  "nodeValue#=": Js_OO.Meth.arity1<string => unit>,
  "style": style,
  "value": Js.undefined<string>,
  "value#=": Js_OO.Meth.arity1<Js.undefined<string> => unit>,
}

@val external document_node: t = "document"

type event = Web_event.t<t>

type event_cb = Web_event.cb<t>

@get_index external getProp_asEventListener: (t, 'key) => Js.undefined<Web_event.cb<t>> = ""

@set_index external setProp_asEventListener: (t, 'key, Js.undefined<Web_event.cb<t>>) => unit = ""

@get_index external getProp: (t, 'key) => 'value = ""

@set_index external setProp: (t, 'key, 'value) => unit = ""

let style: {.."style": 'msg} => 'msg

let getStyle: ({.."style": style}, string) => Js.null<string>

let setStyle: ({.."style": style}, string, Js.null<string>) => unit

let setStyleProperty: ({.."style": style}, ~priority: bool=?, string, Js.null<string>) => unit

let childNodes: {.."childNodes": 'msg} => 'msg

let firstChild: {.."firstChild": 'msg} => 'msg

let appendChild: (t, t) => t

let removeChild: (t, t) => t

let insertBefore: (t, t, t) => t

let remove: (t, unit) => unit

let setAttributeNS: (t, string, string, string) => unit

let setAttribute: (t, string, string) => unit

let setAttributeNsOptional: (t, string, string, string) => unit

let removeAttributeNS: (t, string, string) => unit

let removeAttribute: (t, string) => unit

let removeAttributeNsOptional: (t, string, string) => unit

let addEventListener: (t, string, Web_event.cb<t>, Web_event.options) => unit

let removeEventListener: (t, string, Web_event.cb<t>, Web_event.options) => unit

let focus: t => unit

let set_nodeValue: ({.."nodeValue#=": Js_OO.Meth.arity1<'msg => unit>}, 'msg) => unit

let get_nodeValue: {.."nodeValue": 'msg} => 'msg

let remove_polyfill: unit => unit
