/* https://gist.github.com/pablohirafuji/fa373d07c42016756d5bca28962008c4 */

open Tea
open Tea.Html.Events

module Progress = Http.Progress

type model = {
  progress: Progress.t,
  bookUrl: option<string>,
  bookContent: string,
}

let initModel = {
  progress: Progress.emptyProgress,
  bookUrl: None,
  bookContent: "",
}

let init = () => (initModel, Cmd.none)

type msg =
  | NoOp
  | GetBook(string)
  | GetBookProgress(string, Progress.t)
  | GetBookDone(string, string)

let subscriptions = _model => Sub.none

let progressHelper = b => {
  open Progress
  {bytes: b, bytesExpected: 1}
}

let update = (model, x) =>
  switch x {
  | NoOp => (model, Cmd.none)

  | GetBook(url) =>
    let httpCmd =
      Http.getString(url)
      |> Http.Progress.track(progress => GetBookProgress(url, progress))
      |> Http.send(x =>
        switch x {
        | Error(_e) => NoOp
        | Ok(output) => GetBookDone(url, output)
        }
      )
    (
      {
        ...model,
        bookUrl: Some(url),
        progress: progressHelper(0),
      },
      httpCmd,
    )

  | GetBookProgress(url, progress) =>
    if Some(url) != model.bookUrl {
      (model, Cmd.none)
    } else {
      ({...model, progress: progress}, Cmd.none)
    }

  | GetBookDone(url, bookContent) =>
    if Some(url) != model.bookUrl {
      (model, Cmd.none)
    } else {
      ({...model, bookContent: bookContent, progress: progressHelper(1)}, Cmd.none)
    }
  }

let viewStyle = {
  open Tea.Html.Attributes
  styles(list{
    ("display", "flex"),
    ("flex-direction", "column"),
    ("width", "100%"),
    ("margin", "0 auto"),
    ("font-family", "Arial"),
  })
}

let bookTextViewStyle = {
  open Html.Attributes
  styles(list{("height", "400px"), ("width", "100%")})
}

let inputRadio = (labelText, url) => {
  open Tea_html
  open Tea_html.Attributes
  div(
    list{},
    list{
      label(
        list{
          onCheck(isChecked =>
            if isChecked {
              GetBook(url)
            } else {
              NoOp
            }
          ),
        },
        list{input'(list{type'("radio"), name("book-radio")}, list{}), text(labelText)},
      ),
    },
  )
}

let progressView = loaded => {
  open Tea.Html
  open Tea.Html.Attributes
  div(
    list{},
    list{
      span(list{}, list{text("Progress: ")}),
      progress(list{value(loaded), Attributes.max("100")}, list{text(loaded ++ "%")}),
      text(loaded ++ "%"),
    },
  )
}

let progressLoaded = progress => {
  open Progress
  let bytes = float_of_int(progress.bytes)
  let bytesExpected = float_of_int(progress.bytesExpected)
  if bytesExpected <= 0.0 {
    100
  } else {
    int_of_float(100.0 *. (bytes /. bytesExpected))
  }
}

let footerView = {
  open Tea.Html
  open Tea.Html.Attributes
  span(
    list{},
    list{
      text("Books from "),
      a(list{href("http://www.gutenberg.org/"), target("_blank")}, list{text("Project Gutenberg")}),
    },
  )
}

let bookTextView = valueText => {
  open Tea.Html
  open Tea.Html.Attributes
  div(
    list{},
    list{textarea(list{value(valueText), Attributes.disabled(true), bookTextViewStyle}, list{})},
  )
}

let view = model => {
  open Html
  div(
    list{viewStyle},
    list{
      h1(list{}, list{text("Book Reader")}),
      p(
        list{},
        list{
          text("Select a book:"),
          inputRadio(
            "Essays - Ralph Waldo Emerson",
            "https://s3-sa-east-1.amazonaws.com/estadistas/Essays-Ralph-Waldo-Emerson.txt",
          ),
          inputRadio(
            "Leviathan - Thomas Hobbes",
            "https://s3-sa-east-1.amazonaws.com/estadistas/Leviathan.txt",
          ),
          inputRadio(
            "The Ethics of Aristotle - Aristotle",
            "https://s3-sa-east-1.amazonaws.com/estadistas/The-Ethics-of+Aristotle.txt",
          ),
        },
      ),
      progressView(Belt.Int.toString(progressLoaded(model.progress))),
      bookTextView(model.bookContent),
      footerView,
    },
  )
}

let main = {
  open App
  standardProgram({
    init: init,
    update: update,
    view: view,
    subscriptions: subscriptions,
  })
}
