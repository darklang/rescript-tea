let resultToOption = x =>
  switch x {
  | Ok(a) => Some(a)
  | Error(_) => None
  }

let first = (fst, x) =>
  switch x {
  | Error(_) as e => e
  | Ok(_) => fst
  }

let errorOfFirst = (fst, x) =>
  switch x {
  | Error(e) => Some(e)
  | Ok(_) =>
    switch fst {
    | Ok(_) => None
    | Error(e) => Some(e)
    }
  }
