/* type target = <
  value : string Js.undefined [@bs.get];
> Js.t */

type t<'node> = {
  @get
  "target": Js.undefined<'node>,
  @get
  "keyCode": int,
  @meth
  "preventDefault": unit => unit,
  @meth
  "stopPropagation": unit => unit,
}

type cb<'node> = (. t<'node>) => unit

type options = bool /* false | true (* TODO:  Define a javascript record as another option *) */

type popstateEvent = {.}

type popstateCb = (. popstateEvent) => unit
