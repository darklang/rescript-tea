type unresolved

type xmlHttpRequestUpload

type eventReadystatechange = Js.Json.t
type eventAbort = Js.Json.t
type eventError = Js.Json.t
type eventLoad = Js.Json.t
type eventLoadstart = Js.Json.t
type eventProgress = Js.Json.t
type eventTimeout = Js.Json.t
type eventLoadend = Js.Json.t

@obj
type _xmlhttprequest = {
  // Properties

  @get @set
  "onreadystatechange": eventReadystatechange => unit,
  @get
  "readyState": int,
  @get @set
  "responseType": string,
  "response": Js.null<unresolved>,
  @get
  "responseText": string,
  @get
  "responseURL": string,
  @get
  "responseXML": Js.null<Dom.document>,
  @get
  "status": int,
  @get
  "statusText": string,
  @get @set
  "timeout": float,
  @get
  "upload": xmlHttpRequestUpload,
  @get @set
  "withCredentials": bool,
  // Base events

  @get @set
  "onabort": eventAbort => unit,
  @get @set
  "onerror": eventError => unit,
  @get @set
  "onload": eventLoad => unit,
  @get @set
  "onloadstart": eventLoadstart => unit,
  @get @set
  "onprogress": eventProgress => unit,
  @get @set
  "ontimeout": eventTimeout => unit,
  @get @set
  "onloadend": eventLoadend => unit,
}

type t = _xmlhttprequest

@send external abort: t => unit = "abort"
@send external getAllResponseHeaders: t => Js.null<string> = "getAllResponseHeaders"
@send external getResponseHeader: t => Js.null<string> = "getResponseHeader"
@send external open': (t, string, string, bool, string, string) => unit = "open"
@send external overrideMimeType: (t, string) => unit = "overrideMimeType"
@send external send: t => unit = "send"
@send external sendString: (t, Js.null<string>) => unit = "send"
@send external sendFormData: (t, Webapi.FormData.t) => unit = "send"
@send external sendDocument: (t, Dom.document) => unit = "send"
// send__arrayBuffer: arrayBuffer => unit,
// send__blob: blob => unit,
@send external setRequestHeader: (t, string, string) => unit = "setRequestHeader"

@new external create: unit => t = "XMLHttpRequest"

type errors =
  | IncompleteResponse
  | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody(string)
  | FormDataBody(Webapi.FormData.t)
  | FormListBody(list<(string, string)>)
  | DocumentBody(Dom.document)
/* | BlobBody of Web_blob.t */
/* | ArrayBufferViewBody of Web_arraybuffer_view.t */

/* Main interface functions */

let abort = (x: t): unit => x->abort

let getAllResponseHeaders = (x: t): result<string, errors> =>
  switch Js.Null.toOption(x->getAllResponseHeaders) {
  | None => Error(IncompleteResponse)
  | Some("") => Error(NetworkError)
  | Some(s) => Ok(s)
  }

let getAllResponseHeadersAsList = (x: t): result<list<(string, string)>, errors> =>
  switch getAllResponseHeaders(x) {
  | Error(_) as err => err
  | Ok(s) =>
    Ok(
      s
      |> Js.String.split("\r\n")
      |> Array.map(Js.String.splitAtMost(": ", ~limit=2))
      |> Array.to_list
      |> List.filter(a => Array.length(a) === 2)
      |> List.map(x =>
        switch x {
        | [key, value] => (key, value)
        | _ => failwith("Cannot happen, already checked length")
        }
      ),
    )
  }

let getAllResponseHeadersAsDict = (x: t): result<Belt.Map.String.t<string>, errors> => {
  module StringMap = Belt.Map.String
  switch getAllResponseHeadersAsList(x) {
  | Error(_) as err => err
  | Ok(l) =>
    let insert = (d, (k, v)) => StringMap.set(d, k, v)
    Ok(List.fold_left(insert, StringMap.empty, l))
  }
}

let open' = (method': string, url: string, ~async=true, ~user="", ~password="", x: t) =>
  x->open'(method', url, async, user, password)

let overrideMimeType = (mimetype: string, x: t): unit => overrideMimeType(x, mimetype)

let send = (body: body, x: t): unit =>
  switch body {
  | EmptyBody => x->send
  | EmptyStringBody => x->sendString(Js.Null.empty)
  | StringBody(s) => x->sendString(Js.Null.return(s))
  | FormDataBody(f) => x->sendFormData(f)
  | FormListBody(l) =>
    let form = List.fold_left((f, (key, value)) => {
      let () = Webapi.FormData.append(f, key, value)
      f
    }, Webapi.FormData.make(), l)
    x->sendFormData(form)
  | DocumentBody(d) => x->sendDocument(d)
  }
/* | BlobBody b -> x##send_blob b */
/* | ArrayBufferViewBody a -> x##send_arrayBufferView a */

let setRequestHeader = (header: string, value: string, x: t) => x->setRequestHeader(header, value)

/* Properties */

type state =
  | Unsent
  | Opened
  | HeadersReceived
  | Loading
  | Done

type responseType =
  | StringResponseType
  | ArrayBufferResponseType
  | BlobResponseType
  | DocumentResponseType
  | JsonResponseType
  | TextResponseType
  | RawResponseType(string)

type responseBody =
  | NoResponse
  | StringResponse(string)
  | ArrayBufferResponse(unit)
  | BlobResponse(unit)
  | DocumentResponse(Dom.document)
  | JsonResponse(Js.Json.t)
  | TextResponse(string)
  | RawResponse(string, unit)

let set_onreadystatechange = (cb: eventReadystatechange => unit, x: t): unit =>
  x["onreadystatechange"] = cb

let get_onreadystatechange = (x: t): (eventReadystatechange => unit) => x["onreadystatechange"]

let readyState = (x: t): state =>
  switch x["readyState"] {
  | 0 => Unsent
  | 1 => Opened
  | 2 => HeadersReceived
  | 3 => Loading
  | 4 => Done
  | i => failwith("Invalid return from 'readystate' of: " ++ Belt.Int.toString(i))
  }

let set_responseType = (typ: responseType, x: t): unit =>
  switch typ {
  | StringResponseType => x["responseType"] = ""
  | ArrayBufferResponseType => x["responseType"] = "arraybuffer"
  | BlobResponseType => x["responseType"] = "blob"
  | DocumentResponseType => x["responseType"] = "document"
  | JsonResponseType => x["responseType"] = "json"
  | TextResponseType => x["responseType"] = "text"
  | RawResponseType(s) => x["responseType"] = s
  }

let get_responseType = (x: t): responseType =>
  switch x["responseType"] {
  | "" => StringResponseType
  | "arraybuffer" => ArrayBufferResponseType
  | "blob" => BlobResponseType
  | "document" => DocumentResponseType
  | "json" => JsonResponseType
  | "text" => TextResponseType
  | s => RawResponseType(s)
  }

let get_response = (x: t): responseBody =>
  switch Js.Null.toOption(x["response"]) {
  | None => NoResponse
  | Some(resp) =>
    switch get_responseType(x) {
    | StringResponseType => StringResponse(Obj.magic(resp))
    | ArrayBufferResponseType => ArrayBufferResponse(Obj.magic(resp))
    | BlobResponseType => BlobResponse(Obj.magic(resp))
    | DocumentResponseType => DocumentResponse(Obj.magic(resp))
    | JsonResponseType => JsonResponse(Obj.magic(resp))
    | TextResponseType => TextResponse(Obj.magic(resp))
    | RawResponseType(s) => RawResponse(s, Obj.magic(resp))
    }
  }

let get_responseText = (x: t): string => x["responseText"]

let get_responseURL = (x: t): string => x["responseURL"]

let get_responseXML = (x: t): option<Dom.document> => Js.Null.toOption(x["responseXML"])

let get_status = (x: t): int => x["status"]

let get_statusText = (x: t): string => x["statusText"]

let set_timeout = (t: float, x: t): unit => x["timeout"] = t

let get_timeout = (x: t): float => x["timeout"]

let set_withCredentials = (b: bool, x: t): unit => x["withCredentials"] = b

let get_withCredentials = (x: t): bool => x["withCredentials"]

let set_onabort = (cb: eventAbort => unit, x: t): unit => x["onabort"] = cb

let get_onabort = (x: t): (eventAbort => unit) => x["onabort"]

let set_onerror = (cb: eventError => unit, x: t): unit => x["onerror"] = cb

let get_onerror = (x: t): (eventError => unit) => x["onerror"]

let set_onload = (cb: eventLoad => unit, x: t): unit => x["onload"] = cb

let get_onload = (x: t): (eventLoad => unit) => x["onload"]

let set_onloadstart = (cb: eventLoadstart => unit, x: t): unit => x["onloadstart"] = cb

let get_onloadstart = (x: t): (eventLoadstart => unit) => x["onloadstart"]

let set_onprogress = (cb: eventLoadstart => unit, x: t): unit => x["onprogress"] = cb

let get_onprogress = (x: t): (eventLoadstart => unit) => x["onprogress"]

let set_ontimeout = (cb: eventTimeout => unit, x: t): unit => x["ontimeout"] = cb

let get_ontimeout = (x: t): (eventTimeout => unit) => x["ontimeout"]

let set_onloadend = (cb: eventLoadend => unit, x: t): unit => x["onloadend"] = cb

let get_onloadend = (x: t): (eventLoadend => unit) => x["onloadend"]
