open Tea.App
open Tea.Html
open Tea.Html.Attributes
open Tea.Html.Events


type msg = NewContent(string)

let update = (_oldContent, NewContent(content)) => content

let myStyle = styles(list{
  ("width", "100%"),
  ("height", "40px"),
  ("padding", "10px 0"),
  ("font-size", "2em"),
  ("text-align", "center"),
})

let view = content => {
  let string_rev = s => {
    let len = String.length(s)
    String.init(len, i => String.get(s, len - 1 - i))
  }
  div(
    list{},
    list{
      input'(list{placeholder("Text to reverse"), onInput(s => NewContent(s)), myStyle}, list{}),
      div(list{myStyle}, list{text(string_rev(content))}),
    },
  )
}

let main = beginnerProgram({model: "", view: view, update: update})
