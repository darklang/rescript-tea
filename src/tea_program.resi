type processMsg<'msg> = PushMsg('msg) | Kill

let spawn: ('msg, ('msg, 'b) => option<'msg>, 'msg => unit, processMsg<'b>) => unit

module type Process = {
  type msg
  let handleMsg: msg => unit
}

let testing1: int
