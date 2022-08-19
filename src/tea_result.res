let first = (fst, x) =>
  switch x {
  | Error(_) as e => e
  | Ok(_) => fst
  }