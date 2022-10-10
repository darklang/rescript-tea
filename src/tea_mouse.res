type position = {
  x: int,
  y: int,
}

open JsonCombinators
let position = Json.Decode.object(field => {
    x: field.required(. "pageX", Json.Decode.int),
    y: field.required(. "pageY", Json.Decode.int),
  })

let registerGlobal = (name, key, tagger) => {
  open Vdom
  let enableCall = callbacks_base => {
    let callbacks = ref(callbacks_base)
    let fn = ev => {
      switch Rescript_json_combinators_extended.decodeEvent(position, ev) {
      | Error(_) => None
      | Ok(pos) => Some(tagger(pos))
      }
    }
    let handler = EventHandlerCallback(key, fn)
    let elem = Web_node.document_node
    let cache = eventHandlerRegister(callbacks, elem, name, handler)
    () => {
      let _ = eventHandlerUnregister(elem, name, cache)
    }
  }
  Tea_sub.registration(key, enableCall)
}

let clicks = (~key="", tagger) => registerGlobal("click", key, tagger)

let moves = (~key="", tagger) => registerGlobal("mousemove", key, tagger)

let downs = (~key="", tagger) => registerGlobal("mousedown", key, tagger)

let ups = (~key="", tagger) => registerGlobal("mouseup", key, tagger)
