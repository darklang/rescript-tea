open Tea.App
open Tea.Html
open Tea.Html.Attributes
open Tea.Html.Events



/* MODEL */

type model = {
  name: string,
  password: string,
  passwordAgain: string,
}

let model = {
  name: "",
  password: "",
  passwordAgain: "",
}

/* UPDATE */

type msg =
  | Name(string)
  | Password(string)
  | PasswordAgain(string)

let update = (model, x) =>
  switch x {
  | Name(name) => {...model, name: name}

  | Password(password) => {...model, password: password}

  | PasswordAgain(passwordAgain) => {...model, passwordAgain: passwordAgain}
  }

/* VIEW */

let viewValidation = model => {
  let (color, message) = if model.password === model.passwordAgain {
    ("green", "OK")
  } else {
    ("red", "Passwords do not match!")
  }

  div(list{styles(list{("color", color)})}, list{text(message)})
}

let view = model =>
  div(
    list{},
    list{
      input'(list{type'("text"), placeholder("Name"), onInput(s => Name(s))}, list{}),
      input'(list{type'("password"), placeholder("Password"), onInput(s => Password(s))}, list{}),
      input'(
        list{type'("password"), placeholder("Re-enter Password"), onInput(s => PasswordAgain(s))},
        list{},
      ),
      viewValidation(model),
    },
  )

let main = beginnerProgram({
  model: model,
  view: view,
  update: update,
})
