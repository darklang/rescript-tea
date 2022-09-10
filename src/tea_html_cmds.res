let focus = id =>
  Tea_cmd.call(_enqueue => {
    let ecb = _ =>
      switch (Webapi.Dom.Document.getElementById(Webapi.Dom.document,id)->Belt.Option.flatMap(Webapi.Dom.HtmlInputElement.ofElement)){
      | None => Js.log(("Attempted to focus a non-existant element of: ", id))
      | Some(elem) => Webapi.Dom.HtmlInputElement.focus(elem)
      }

    /* One to get out of the current render frame */
    let cb = _ => ignore(Webapi.requestAnimationFrame(ecb))
    /* And another to properly focus */
    ignore(Webapi.requestAnimationFrame(cb))
    ()
  })
