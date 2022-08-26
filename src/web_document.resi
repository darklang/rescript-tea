type t = {"body": Web_node.t, "location": Web_location.t}

@val external document: t = "document"

let body: unit => Web_node.t

let createElement: string => Web_node.t

let createElementNS: (string, string) => Web_node.t

let createComment: string => Web_node.t

let createTextNode: string => Web_node.t

let getElementById: string => Js.null_undefined<Web_node.t>

let createElementNsOptional: (string, string) => Web_node.t

let location: unit => Web_location.t

