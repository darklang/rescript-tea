
open Webapi
open Webapi.Dom
open Document

let focus = id =>
  Tea_cmd.call(_enqueue => {
    let ecb = _ =>
      switch (getElementById(document,id)->Belt.Option.flatMap(HtmlInputElement.ofElement)){
      | None => Js.log(("Attempted to focus a non-existant element of: ", id))
      | Some(elem) => HtmlInputElement.focus(elem)
      }

    /* One to get out of the current render frame */
    let cb = _ => ignore(requestAnimationFrame(ecb))
    /* And another to properly focus */
    ignore(requestAnimationFrame(cb))
    ()
  })
