type systemMessage<'msg> = Render | AddRenderMsg('msg) | RemoveRenderMsg('msg)

type applicationCallbacks<'msg> = {enqueue: 'msg => unit, on: systemMessage<'msg> => unit}

type eventHandler<'msg> =
  | EventHandlerCallback(string, Web.Node.event => option<'msg>)
  | EventHandlerMsg('msg)

type eventCache<'msg> = {handler: Web.Node.event_cb, cb: ref<Web.Node.event => option<'msg>>}

type property<'msg> =
  | NoProp
  | RawProp(string, string)
  | Attribute(string, string, string)
  | Data(string, string)
  | Event(string, eventHandler<'msg>, ref<option<eventCache<'msg>>>)
  | Style(list<(string, string)>)

type properties<'msg> = list<property<'msg>>

type rec t<'msg> =
  | CommentNode(string)
  | Text(string)
  | Node(string, string, string, string, properties<'msg>, list<t<'msg>>)
  | LazyGen(string, unit => t<'msg>, ref<t<'msg>>)
  | Tagger(ref<applicationCallbacks<'msg>> => ref<applicationCallbacks<'msg>>, t<'msg>)

let noNode: t<'msg>

let comment: string => t<'msg>

let text: string => t<'msg>

let fullnode: (string, string, string, string, properties<'msg>, list<t<'msg>>) => t<'msg>

let node: (
  ~namespace: string=?,
  string,
  ~key: string=?,
  ~unique: string=?,
  properties<'msg>,
  list<t<'msg>>,
) => t<'msg>

let lazyGen: (string, unit => t<'msg>) => t<'msg>

let noProp: property<'msg>

let prop: (string, string) => property<'msg>

let onCB: (string, string, Web.Node.event => option<'msg>) => property<'msg>

let onMsg: (string, 'msg) => property<'msg>

let attribute: (string, string, string) => property<'msg>

let data: (string, string) => property<'msg>

let style: (string, string) => property<'msg>

let styles: list<(string, string)> => property<'msg>

let renderToHtmlString: t<'msg> => string

let emptyEventHandler: Web.Node.event_cb

let emptyEventCB: 'msg => option<Web.Node.event_cb>

let eventHandler: (
  ref<applicationCallbacks<'msg>>,
  ref<Web.Node.event => option<'msg>>,
) => Web.Node.event_cb

let eventHandler_GetCB: (eventHandler<'msg>, Web.Node.event) => option<'msg>

let compareEventHandlerTypes: (eventHandler<'msg>, eventHandler<'msg>) => bool

let eventHandler_Register: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  string,
  eventHandler<'msg>,
) => option<eventCache<'msg>>

let eventHandler_Unregister: (Web.Node.t, string, option<eventCache<'msg>>) => option<
  eventCache<'msg>,>

let eventHandler_Mutate: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  string,
  string,
  eventHandler<'msg>,
  eventHandler<'msg>,
  ref<option<eventCache<'msg>>>,
  ref<option<eventCache<'msg>>>,
) => unit

let patchVNodesOnElems_PropertiesApply_Add: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  int,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply_Remove: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  int,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply_RemoveAdd: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  int,
  property<'msg>,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply_Mutate: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  int,
  property<'msg>,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  int,
  list<property<'msg>>,
  list<property<'msg>>,
) => bool

let patchVNodesOnElems_Properties: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  list<property<'msg>>,
  list<property<'msg>>,
) => bool

let genEmptyProps: int => list<property<'msg>>

let mapEmptyProps: list<property<'msg>> => list<property<'msg>>

let patchVNodesOnElems_ReplaceNode: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  array<Web.Node.t>,
  int,
  t<'msg>,
) => unit

let patchVNodesOnElems_CreateElement: (ref<applicationCallbacks<'msg>>, t<'msg>) => Web.Node.t

let patchVNodesOnElems_MutateNode: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  array<Web.Node.t>,
  int,
  t<'msg>,
  t<'msg>,
) => unit

let patchVNodesOnElems: (
  ref<applicationCallbacks<'msg>>,
  Web_node.t,
  Js.Array.t<Web_node.t>,
  int,
  list<t<'msg>>,
  list<t<'msg>>,
) => unit

let patchVNodesIntoElement: (
  ref<applicationCallbacks<'msg>>,
  Web.Node.t,
  list<t<'msg>>,
  list<t<'msg>>,
) => list<t<'msg>>

let patchVNodeIntoElement: (ref<applicationCallbacks<'msg>>, Web.Node.t, t<'msg>, t<'msg>) => list<
  t<'msg>,>

let wrapCallbacks_On: ('msg => 'b, systemMessage<'msg>) => systemMessage<'b>

let wrapCallbacks: ('msg => 'b, ref<applicationCallbacks<'b>>) => ref<applicationCallbacks<'msg>>

let map: ('msg => 'b, t<'msg>) => t<'b>
