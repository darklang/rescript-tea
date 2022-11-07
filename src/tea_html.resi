let map: ('msg => 'b, Vdom.t<'msg>) => Vdom.t<'b>

let text: string => Vdom.t<'msg>

let node: (
  ~namespace: string=?,
  string,
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let noNode: Vdom.t<'msg>

let lazy1: (string, unit => Vdom.t<'msg>) => Vdom.t<'msg>

let h1: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let h2: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let h3: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let h4: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let h5: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let h6: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let div: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let p: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let hr: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let pre: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let blockquote: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let span: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let a: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let code: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let em: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let strong: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let i: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let b: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let u: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let sub: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let sup: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let br: Vdom.properties<'msg> => Vdom.t<'msg>

let br': (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let ol: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let ul: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let li: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let dl: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let dt: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let dd: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let img: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let iframe: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let canvas: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let math: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let form: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

// Named input' to not clash with Pervasives.input when open
let input': (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let textarea: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let button: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let select: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let option: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let optgroup: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let label: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fieldset: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let legend: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let section: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let nav: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let article: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let aside: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let header: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let footer: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let address: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let main: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let body: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let figure: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let figcaption: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let table: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let caption: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let colgroup: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let col: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let tbody: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let thead: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let tfoot: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let tr: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let th: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let td: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let datalist: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let keygen: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let output: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let progress: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let meter: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let audio: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let video: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let source: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let track: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let embed: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let object: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let param: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let ins: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let del: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let small: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let cite: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let dfn: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let abbr: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let time: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let var: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let samp: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let kbd: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let s: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let q: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let mark: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let ruby: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let rt: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let rp: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let bdi: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let bdo: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let wbr: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let details: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let summary: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let menuitem: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let menu: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let meta: (~key: string=?, ~unique: string=?, Vdom.properties<'msg>) => Vdom.t<'msg>

let style: (~key: string=?, ~unique: string=?, Vdom.properties<'msg>, string) => Vdom.t<'msg>

let title: (~key: string=?, ~unique: string=?, Vdom.properties<'msg>, string) => Vdom.t<'msg>

let link: (~key: string=?, ~unique: string=?, Vdom.properties<'msg>) => Vdom.t<'msg>

module Attributes: {
  let noProp: Vdom.property<'msg>

  let style: (string, string) => Vdom.property<'msg>

  let styles: list<(string, string)> => Vdom.property<'msg>

  let class: string => Vdom.property<'msg>

  let classList: list<(string, bool)> => Vdom.property<'msg>

  let id: string => Vdom.property<'msg>

  let title: string => Vdom.property<'msg>

  let hidden: bool => Vdom.property<'msg>

  let type': string => Vdom.property<'msg>

  let value: string => Vdom.property<'msg>

  let defaultValue: string => Vdom.property<'msg>

  let checked: bool => Vdom.property<'msg>

  let placeholder: string => Vdom.property<'msg>

  let selected: bool => Vdom.property<'msg>

  let accept: string => Vdom.property<'msg>

  let acceptCharset: string => Vdom.property<'msg>

  let action: string => Vdom.property<'msg>

  let autocomplete: bool => Vdom.property<'msg>

  let autofocus: bool => Vdom.property<'msg>

  let disabled: bool => Vdom.property<'msg>

  let enctype: string => Vdom.property<'msg>

  let formaction: string => Vdom.property<'msg>

  let list: string => Vdom.property<'msg>

  let minlength: int => Vdom.property<'msg>

  let maxlength: int => Vdom.property<'msg>

  let method: string => Vdom.property<'msg>

  let multiple: bool => Vdom.property<'msg>

  let name: string => Vdom.property<'msg>

  let novalidate: bool => Vdom.property<'msg>

  let pattern: string => Vdom.property<'msg>

  let readonly: bool => Vdom.property<'msg>

  let required: bool => Vdom.property<'msg>

  let size: int => Vdom.property<'msg>

  let for': string => Vdom.property<'msg>

  let form: string => Vdom.property<'msg>

  let max: string => Vdom.property<'msg>

  let min: string => Vdom.property<'msg>

  let step: string => Vdom.property<'msg>

  let cols: int => Vdom.property<'msg>

  let rows: int => Vdom.property<'msg>

  let wrap: string => Vdom.property<'msg>

  let href: string => Vdom.property<'msg>

  let target: string => Vdom.property<'msg>

  let download: bool => Vdom.property<'msg>

  let downloadAs: string => Vdom.property<'msg>

  let hreflang: string => Vdom.property<'msg>

  let media: string => Vdom.property<'msg>

  let ping: string => Vdom.property<'msg>

  let rel: string => Vdom.property<'msg>

  let ismap: bool => Vdom.property<'msg>

  let usemap: string => Vdom.property<'msg>

  let shape: string => Vdom.property<'msg>

  let coords: string => Vdom.property<'msg>

  let src: string => Vdom.property<'msg>

  let height: int => Vdom.property<'msg>

  let width: int => Vdom.property<'msg>

  let alt: string => Vdom.property<'msg>

  let autoplay: bool => Vdom.property<'msg>

  let controls: bool => Vdom.property<'msg>

  let loop: bool => Vdom.property<'msg>

  let preload: string => Vdom.property<'msg>

  let poster: string => Vdom.property<'msg>

  let default: bool => Vdom.property<'msg>

  let kind: string => Vdom.property<'msg>

  let srclang: string => Vdom.property<'msg>

  let sandbox: string => Vdom.property<'msg>

  let seamless: bool => Vdom.property<'msg>

  let srcdoc: string => Vdom.property<'msg>

  let reversed: bool => Vdom.property<'msg>

  let start: int => Vdom.property<'msg>

  let colspan: int => Vdom.property<'msg>

  let rowspan: int => Vdom.property<'msg>

  let headers: string => Vdom.property<'msg>

  let scope: string => Vdom.property<'msg>

  let align: string => Vdom.property<'msg>

  let async: bool => Vdom.property<'msg>

  let charset: string => Vdom.property<'msg>

  let content: string => Vdom.property<'msg>

  let defer: bool => Vdom.property<'msg>

  let httpEquiv: string => Vdom.property<'msg>

  let language: string => Vdom.property<'msg>

  let scoped: string => Vdom.property<'msg>

  let accesskey: char => Vdom.property<'msg>

  let contenteditable: bool => Vdom.property<'msg>

  let contextmenu: string => Vdom.property<'msg>

  let dir: string => Vdom.property<'msg>

  let draggable: string => Vdom.property<'msg>

  let dropzone: string => Vdom.property<'msg>

  let itemprop: string => Vdom.property<'msg>

  let lang: string => Vdom.property<'msg>

  let spellcheck: bool => Vdom.property<'msg>

  let tabindex: int => Vdom.property<'msg>

  let challenge: string => Vdom.property<'msg>

  let keytype: string => Vdom.property<'msg>

  let cite: string => Vdom.property<'msg>

  let datetime: string => Vdom.property<'msg>

  let pubdate: string => Vdom.property<'msg>

  let manifest: string => Vdom.property<'msg>

  let role: string => Vdom.property<'msg>

  let ariaChecked: bool => Vdom.property<'msg>

  let ariaHidden: bool => Vdom.property<'msg>
}
module Events: {
  type options = {stopPropagation: bool, preventDefault: bool}

  let onCB: (~key: string, string, Dom.event => option<'msg>) => Vdom.property<'msg>

  let onMsg: (string, 'msg) => Vdom.property<'msg>

  let on: (~key: string, string, Tea_json.Decoder.t<'event, 'msg>) => Vdom.property<'msg>

  let onWithOptions: (
    ~key: string,
    string,
    options,
    Tea_json.Decoder.t<'event, 'msg>,
  ) => Vdom.property<'msg>

  let defaultOptions: options

  let targetValue: Tea_json.Decoder.t<Js.Json.t, string>

  let targetChecked: Tea_json.Decoder.t<Js.Json.t, bool>

  let keyCode: Tea_json.Decoder.t<Js.Json.t, int>

  let preventDefaultOn: (
    ~key: string=?,
    string,
    Tea_json.Decoder.t<'event, 'msg>,
  ) => Vdom.property<'msg>

  let onClick: 'msg => Vdom.property<'msg>

  let onDoubleClick: 'msg => Vdom.property<'msg>

  let onMouseDown: 'msg => Vdom.property<'msg>

  let onMouseUp: 'msg => Vdom.property<'msg>

  let onMouseEnter: 'msg => Vdom.property<'msg>

  let onMouseLeave: 'msg => Vdom.property<'msg>

  let onMouseOver: 'msg => Vdom.property<'msg>

  let onMouseOut: 'msg => Vdom.property<'msg>

  let onInputOpt: (~key: string=?, string => option<'msg>) => Vdom.property<'msg>

  let onInput: (~key: string=?, string => 'msg) => Vdom.property<'msg>

  let onCheckOpt: (~key: string=?, bool => option<'msg>) => Vdom.property<'msg>

  let onCheck: (~key: string=?, bool => 'msg) => Vdom.property<'msg>

  let onChangeOpt: (~key: string=?, string => option<'msg>) => Vdom.property<'msg>

  let onChange: (~key: string=?, string => 'msg) => Vdom.property<'msg>

  let onSubmit: 'msg => Vdom.property<'msg>

  let onBlur: 'msg => Vdom.property<'msg>

  let onFocus: 'msg => Vdom.property<'msg>
}
