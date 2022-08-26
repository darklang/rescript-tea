type _formdata = unit

type t = _formdata

@new external create: unit => t = "FormData"

let append: (string, string, unit) => unit
