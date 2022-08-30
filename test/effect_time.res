open Tea

type time = float

type mySub<'msg> = Every(time, time => 'msg)

type myCmd<'msg> = Delay(time, unit => 'msg)

let every = (interval, tagger) => Every(interval, tagger)

let delay = (msTime, msg) =>
  Cmd.call(callbacks => {
    let _unhandledID = Web.Window.setTimeout(() => {
      open Vdom
      callbacks.contents.enqueue(msg)
    }, msTime)
  })
