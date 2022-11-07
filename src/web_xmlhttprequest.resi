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

type _xmlhttprequest = {
  "onabort": eventAbort => unit,
  "onabort#=": Js_OO.Meth.arity1<(eventAbort => unit) => unit>,
  "onerror": eventError => unit,
  "onerror#=": Js_OO.Meth.arity1<(eventError => unit) => unit>,
  "onload": eventLoad => unit,
  "onload#=": Js_OO.Meth.arity1<(eventLoad => unit) => unit>,
  "onloadend": eventLoadend => unit,
  "onloadend#=": Js_OO.Meth.arity1<(eventLoadend => unit) => unit>,
  "onloadstart": eventLoadstart => unit,
  "onloadstart#=": Js_OO.Meth.arity1<(eventLoadstart => unit) => unit>,
  "onprogress": eventProgress => unit,
  "onprogress#=": Js_OO.Meth.arity1<(eventProgress => unit) => unit>,
  "onreadystatechange": eventReadystatechange => unit,
  "onreadystatechange#=": Js_OO.Meth.arity1<(eventReadystatechange => unit) => unit>,
  "ontimeout": eventTimeout => unit,
  "ontimeout#=": Js_OO.Meth.arity1<(eventTimeout => unit) => unit>,
  "readyState": int,
  "response": Js.null<unresolved>,
  "responseText": string,
  "responseType": string,
  "responseType#=": Js_OO.Meth.arity1<string => unit>,
  "responseURL": string,
  "responseXML": Js.null<Dom.document>,
  "status": int,
  "statusText": string,
  "timeout": float,
  "timeout#=": Js_OO.Meth.arity1<float => unit>,
  "upload": xmlHttpRequestUpload,
  "withCredentials": bool,
  "withCredentials#=": Js_OO.Meth.arity1<bool => unit>,
}

type t = _xmlhttprequest

@send external getResponseHeader: t => Js.null<string> = "getResponseHeader"

@send external sendString: (t, Js.null<string>) => unit = "send"

@send external sendFormData: (t, Webapi.FormData.t) => unit = "send"

@send external sendDocument: (t, Dom.document) => unit = "send"

@new external create: unit => t = "XMLHttpRequest"

type errors = IncompleteResponse | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody(string)
  | FormDataBody(Webapi.FormData.t)
  | FormListBody(list<(string, string)>)
  | DocumentBody(Dom.document)

let abort: t => unit

let getAllResponseHeaders: t => result<string, errors>

let getAllResponseHeadersAsList: t => result<list<(string, string)>, errors>

let getAllResponseHeadersAsDict: t => result<Belt.Map.String.t<string>, errors>

let open': (string, string, ~async: bool=?, ~user: string=?, ~password: string=?, t) => unit

let overrideMimeType: (string, t) => unit

let send: (body, t) => unit

let setRequestHeader: (string, string, t) => unit

type state = Unsent | Opened | HeadersReceived | Loading | Done

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

let set_onreadystatechange: (eventReadystatechange => unit, t) => unit

let get_onreadystatechange: (t, eventReadystatechange) => unit

let readyState: t => state

let set_responseType: (responseType, t) => unit

let get_responseType: t => responseType

let get_response: t => responseBody

let get_responseText: t => string

let get_responseURL: t => string

let get_responseXML: t => option<Dom.document>

let get_status: t => int

let get_statusText: t => string

let set_timeout: (float, t) => unit

let get_timeout: t => float

let set_withCredentials: (bool, t) => unit

let get_withCredentials: t => bool

let set_onabort: (eventAbort => unit, t) => unit

let get_onabort: (t, eventAbort) => unit

let set_onerror: (eventError => unit, t) => unit

let get_onerror: (t, eventError) => unit

let set_onload: (eventLoad => unit, t) => unit

let get_onload: (t, eventLoad) => unit

let set_onloadstart: (eventLoadstart => unit, t) => unit

let get_onloadstart: (t, eventLoadstart) => unit

let set_onprogress: (eventLoadstart => unit, t) => unit

let get_onprogress: (t, eventLoadstart) => unit

let set_ontimeout: (eventTimeout => unit, t) => unit

let get_ontimeout: (t, eventTimeout) => unit

let set_onloadend: (eventLoadend => unit, t) => unit

let get_onloadend: (t, eventLoadend) => unit
