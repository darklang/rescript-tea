module History = Web_window_history

module LocalStorage = Web_window_localstorage

type timeoutHandlerID = int

type t = {
  "history": Js.Undefined.t<History.t>,
  "localStorage": Js.Undefined.t<LocalStorage.t>,
  "location": Web_location.t,
}

@val external window: t = "window"

let history: unit => Js.Undefined.t<History.t>

let localStorage: unit => Js.Undefined.t<LocalStorage.t>

let location: unit => Web_location.t

let requestAnimationFrame: (float => unit) => int

let cancelAnimationFrame: int => unit

let clearTimeout: timeoutHandlerID => unit

let setInterval: (unit => unit, float) => timeoutHandlerID

let setTimeout: (unit => unit, float) => timeoutHandlerID

let addEventListener: (string, Web_event.cb<Web_node.t>, Web_event.options) => unit

let removeEventListener: (string, Web_event.cb<Web_node.t>, Web_event.options) => unit

let requestAnimationFrame_polyfill: unit => unit
