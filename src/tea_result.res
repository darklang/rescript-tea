let first = (fst, x) =>
  switch x {
  | Error(_) as e => e
  | Ok(_) => fst
  }


let resultToOption = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }