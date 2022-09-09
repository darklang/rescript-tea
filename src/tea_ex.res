let renderEvent = (~key="", msg) => {
  open Vdom
  let enableCall = callbacks => {
    let () = callbacks.on(AddRenderMsg(msg))
    () => callbacks.on(RemoveRenderMsg(msg))
  }
  Tea_sub.registration(key, enableCall)
}
module LocalStorage = {
  open Tea_task

  let length = nativeBinding(cb =>{
    let value = Dom.Storage.length(Dom.Storage.localStorage)
    if(Js.testAny(value)) {
    cb(Error("localStorage is not available"))}
    else{cb(Ok(value))}
    }
  )

  let clear = nativeBinding(cb =>{
    if(Js.testAny(Dom.Storage.clear(Dom.Storage.localStorage))){
      cb(Error("localStorage is not available"))}
    else{
      let value =Dom.Storage.clear(Dom.Storage.localStorage)
       cb(Ok(value))
    }}
  )
  let clearCmd = () => Tea_task.attemptOpt(_ => None, clear)
  let key = idx =>
    nativeBinding(cb =>
      switch Dom.Storage.key(idx, Dom.Storage.localStorage) {
      | None => cb(Error("localStorage is not available"))
      | Some(value) => cb(Ok(value))
      }
    )
  let getItem = key =>
    nativeBinding(cb =>
      switch Dom.Storage.getItem(key,Dom.Storage.localStorage) {
      | None => cb(Error("localStorage is not available"))
      | Some(value) => cb(Ok(value))
      }
    )

    let removeItem = key =>
    nativeBinding(cb =>{
      let value=Dom.Storage.removeItem(key, Dom.Storage.localStorage)
      if(Js.testAny( value)) {
      cb(Error("localStorage is not available"))}
      else{
        cb(Ok(value))}}
      
    )

    let removeItemCmd = key => Tea_task.attemptOpt(_ => None, removeItem(key))
  let setItem = (key, value) =>
    nativeBinding(cb =>
      if( Js.testAny(Dom.Storage.setItem(key, value, Dom.Storage.localStorage))) {
      cb(Error("localStorage is not available"))}
      else{cb(Ok())}
      
    )
  let setItemCmd = (key, value) => Tea_task.attemptOpt(_ => None, setItem(key, value))
}
