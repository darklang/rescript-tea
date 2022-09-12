let cmd = (promise, tagger) => {
  open Vdom
  Tea_cmd.call(x =>
    switch x {
    | callbacks =>
      let _ = promise |> Js.Promise.then_(x =>
        switch x {
        | res =>
          switch tagger(res) {
          | Some(msg) =>
            let () = callbacks.contents.enqueue(msg)
            Js.Promise.resolve()
          | None => Js.Promise.resolve()
          }
        }
      )
    }
  )
}

let result = (promise, msg) => {
  open Vdom
  Tea_cmd.call(x =>
    switch x {
    | callbacks =>
      let enq = result => callbacks.contents.enqueue(msg(result))

      let _ =
        promise
        |> Js.Promise.then_(x =>
          switch x {
          | res =>
            let resolve = enq(Ok(res))
            Js.Promise.resolve(resolve)
          }
        )
        |> Js.Promise.catch(x =>
          switch x {
          | err =>
            let errToString = err => j`$err`
            let reject = enq(Error(errToString(err)))
            Js.Promise.resolve(reject)
          }
        )
    }
  )
}
