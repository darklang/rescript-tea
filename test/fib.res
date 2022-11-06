let fib = n => {
  let rec aux = (n, a, b) =>
    if n == 0 {
      a
    } else {
      aux(n - 1, b, a + b)
    }
  aux(n, 1, 1)
}
