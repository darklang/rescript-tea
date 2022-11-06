/* let load = Main_counter.main (Web.Document.getElementById "content") () */

/* let load = Main_todo.main (Web.Document.getElementById "content") () */

/* let load = Main_todo_optimized.main (Web.Document.getElementById "content") () */

/* let load = Main_todo_optimizedarray.main (Web.Document.getElementById "content") () */

/* let load = Main_todo_optimizedmap.main (Web.Document.getElementById "content") () */

/* let load = Main_todoarray.main (Web.Document.getElementById "content") () */

/* let load = Main_todo_optimizedarray.main (Web.Document.getElementById "content") () */

/* let () =
  Js.log "inited";
  Js.log "Resetting";
  m Reset;
  Js.log "Increment";
  m Increment;
  Js.log "Increment";
  m Increment;
  Js.log "Set";
  m (Set 42); */

/* let main =
  beginnerPrograms (Js.Null_undefined.return (Web.Document.body ()))
  (* beginnerPrograms (Js.Null_undefined.return (Web.Document.body ())) *) */

/* 
let res =
  for i = 0 to 10 do
    Js.log (Fib.fib i)
  done

module type X_int = sig val x : int end

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Three = struct let x = 3 end

module Four = Increment(Three)

module Five = Increment(Four)

let inc_test = Four.x + Five.x
*/

/* 
let view_test =
  let model = { count = 42; more = "" } in
  view model

let renderTest =
  let v = view_test in
  renderToHtmlString v

let () = Js.log (renderToHtmlString view_test)

let elem = createElementFromVNode view_test

let attachedElem = match Js.Null.to_opt (Web.Document.getElementById "content") with
  | None -> Js.log "Failed to attach"
  | Some e -> let attached = Web.Node.appendChild e elem in Js.log attached
*/

/* let main p =
  let module P = (val (Tea_app.makeProgram p) : Tea_app.ProgramState) in
  P.x */

/* module CounterApp = module Tea.App.MakeProgram Counter */

/* module App =
  Tea.App.Make (struct

  end) */

/* module Main = App.Make (struct
    type flags = unit

    type model =
      { count : int
      }

    type msg =
      | Increment
      | Decrement
      | Reset
      | Set of int
  end) */

/* let _ = App.main struct
          type t = int
        end */
