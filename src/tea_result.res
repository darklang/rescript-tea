let first = (fst, x) =>
  switch x {
  | Error(_) as e => e
  | Ok(_) => fst
  }


let result_to_option = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }