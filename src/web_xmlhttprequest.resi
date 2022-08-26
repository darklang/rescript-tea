type unresolved

type xmlHttpRequestUpload

type event_readystatechange = Web_json.t

type event_abort = Web_json.t

type event_error = Web_json.t

type event_load = Web_json.t

type event_loadstart = Web_json.t

type event_progress = Web_json.t

type event_timeout = Web_json.t

type event_loadend = Web_json.t

type _xmlhttprequest = {
  "onabort": event_abort => unit,
  "onabort#=": Js_OO.Meth.arity1<(event_abort => unit) => unit>,
  "onerror": event_error => unit,
  "onerror#=": Js_OO.Meth.arity1<(event_error => unit) => unit>,
  "onload": event_load => unit,
  "onload#=": Js_OO.Meth.arity1<(event_load => unit) => unit>,
  "onloadend": event_loadend => unit,
  "onloadend#=": Js_OO.Meth.arity1<(event_loadend => unit) => unit>,
  "onloadstart": event_loadstart => unit,
  "onloadstart#=": Js_OO.Meth.arity1<(event_loadstart => unit) => unit>,
  "onprogress": event_loadstart => unit,
  "onprogress#=": Js_OO.Meth.arity1<(event_loadstart => unit) => unit>,
  "onreadystatechange": event_readystatechange => unit,
  "onreadystatechange#=": Js_OO.Meth.arity1<(event_readystatechange => unit) => unit>,
  "ontimeout": event_timeout => unit,
  "ontimeout#=": Js_OO.Meth.arity1<(event_timeout => unit) => unit>,
  "readyState": int,
  "response": Js.null<unresolved>,
  "responseText": string,
  "responseType": string,
  "responseType#=": Js_OO.Meth.arity1<string => unit>,
  "responseURL": string,
  "responseXML": Js.null<Web_document.t>,
  "status": int,
  "statusText": string,
  "timeout": float,
  "timeout#=": Js_OO.Meth.arity1<float => unit>,
  "upload": xmlHttpRequestUpload,
  "withCredentials": bool,
  "withCredentials#=": Js_OO.Meth.arity1<bool => unit>,
}

type t = _xmlhttprequest
    @send external _open: (t,string, string, bool, string, string) => unit="_open"
    @send external send__string: (t,Js.null<string>) => unit="send__string"
    @send external send__formdata : (t,Web_formdata.t) => unit="send__formdata"
    @send external send__document : (t,Web_document.t) => unit="send__document"

@new external create: unit => t = "XMLHttpRequest"

type errors = IncompleteResponse | NetworkError

type body =
  | EmptyBody
  | EmptyStringBody
  | StringBody(string)
  | FormDataBody(Web_formdata.t)
  | FormListBody(list<(string, string)>)
  | DocumentBody(Web_document.t)

let abort: t => unit

let getAllResponseHeaders: t => result<string, errors>

let getAllResponseHeadersAsList: t => result<list<(string, string)>, errors>

let getAllResponseHeadersAsDict: t => result<Belt.Map.String.t<string>, errors>

let getResponseHeader: ('msg, {.."getResponse": 'msg => Js.Null.t<'b>}) => option<'b>

let open_: (string, string, ~async: bool=?, ~user: string=?, ~password: string=?, t) => unit

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
  | DocumentResponse(Web_document.t)
  | JsonResponse(Web_json.t)
  | TextResponse(string)
  | RawResponse(string, unit)

let set_onreadystatechange: (event_readystatechange => unit, t) => unit

let get_onreadystatechange: (t, event_readystatechange) => unit

let readyState: t => state

let set_responseType: (responseType, t) => unit

let get_responseType: t => responseType

let get_response: t => responseBody

let get_responseText: t => string

let get_responseURL: t => string

let get_responseXML: t => option<Web_document.t>

let get_status: t => int

let get_statusText: t => string

let set_timeout: (float, t) => unit

let get_timeout: t => float

let set_withCredentials: (bool, t) => unit

let get_withCredentials: t => bool

let set_onabort: (event_abort => unit, t) => unit

let get_onabort: (t, event_abort) => unit

let set_onerror: (event_error => unit, t) => unit

let get_onerror: (t, event_error) => unit

let set_onload: (event_load => unit, t) => unit

let get_onload: (t, event_load) => unit

let set_onloadstart: (event_loadstart => unit, t) => unit

let get_onloadstart: (t, event_loadstart) => unit

let set_onprogress: (event_loadstart => unit, t) => unit

let get_onprogress: (t, event_loadstart) => unit

let set_ontimeout: (event_timeout => unit, t) => unit

let get_ontimeout: (t, event_timeout) => unit

let set_onloadend: (event_loadend => unit, t) => unit

let get_onloadend: (t, event_loadend) => unit
