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

  let length = nativeBinding(cb => {
    try {
      cb(Ok(Dom.Storage.length(Dom.Storage.localStorage)))
    } catch {
    | Not_found => cb(Error("localStorage is not available"))
    }
  })

  let clear = nativeBinding(cb =>
    try {
      cb(Ok(Dom.Storage.clear(Dom.Storage.localStorage)))
    } catch {
    | Not_found => cb(Error("localStorage is not available"))
    }
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
      switch Dom.Storage.getItem(key, Dom.Storage.localStorage) {
      | None => cb(Error("localStorage is not available"))
      | Some(value) => cb(Ok(value))
      }
    )

  let removeItem = key =>
    nativeBinding(cb =>
      try {
        cb(Ok(Dom.Storage.removeItem(key, Dom.Storage.localStorage)))
      } catch {
      | Not_found => cb(Error("localStorage is not available"))
      }
    )

  let removeItemCmd = key => Tea_task.attemptOpt(_ => None, removeItem(key))
  let setItem = (key, value) =>
    nativeBinding(cb => {
      try {
        cb(Ok(Dom.Storage.setItem(key, value, Dom.Storage.localStorage)))
      } catch {
      | Not_found => cb(Error("localStorage is not available"))
      }
    })
  let setItemCmd = (key, value) => Tea_task.attemptOpt(_ => None, setItem(key, value))
}
