type t<'node> = {
  "keyCode": int,
  "preventDefault": Js_OO.Meth.arity0<unit>,
  "stopPropagation": Js_OO.Meth.arity0<unit>,
  "target": Js.undefined<'node>,
}

type cb<'node> = (. t<'node>) => unit

type options = bool

type popstateEvent = {.}

type popstateCb = (. popstateEvent) => unit
