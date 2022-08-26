type t = {.}

type date_obj

@new external create_date: unit => t = "Date"
@val external date_obj: date_obj = "Date"

let now: unit => float

