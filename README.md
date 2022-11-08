# Rescript-TEA

[![NPM](https://nodei.co/npm/rescript-tea.png?compact=true)](https://nodei.co/npm/rescript-tea/)

[![Build Status](https://circleci.com/gh/darklang/rescript-tea.svg?style=svg)](https://circleci.com/darklang/rescript-tea)

## Description

This is a library that enables The Elm Architecture for Rescript.

[The Elm Architecture](https://guide.elm-lang.org/architecture/) is an MVU pattern
for organizing frontend applications and components. Another example a
TEA-influenced project is React/Redux.

In TEA, each component has single model. The model is updated by receiving messages
(typically named `msg`) - all relevent browser and user events, including keyboard,
mouse, fetch, clipboard, etc, are converted into messages. An `update` method
receives a model and message, and returns a new model. It can also return commands,
which affect the outside world, for example by making API calls. And, that's it.
That's the whole idea.

The model is used to render HTML via a built-in virtual DOM. The entire application
is just a single component with a single model and `update` function.

## Advantages

- Entirely event driven, this is like React/Redux but type-safe and significantly faster.
- Amazingly fast compile-times, especially with Rescript's built-in watcher
- Open license.

You can read more about it [here](http://blog.overminddl1.com/tags/bucklescript-tea/).

## Project design

- [X] Elm API: Following the Elm API as closely as Rescript allows. Converting code back and forth between Elm and OCaml should be made as easy as possible and there exists both a [converter](https://github.com/darklang/philip2), as well as [documentation](https://github.com/darklang/philip2#how-to-port-your-project) for that process.

## Installation

### NPM

First verify you have `rescript` installed, whether globally or just in your project.

Then install via npm by:

```sh
npm install --save-dev rescript-tea
```

Then in your current Rescript project just use this as a dependency add this to your bsconfig.json file:

```json
  "bs-dependencies" : ["rescript-tea"]
```

## Usage

### Example project

Once you have your Rescript project set up and the dependencies configured as above
then lets make a new TEA module, the Counter, as is traditional in Elm tutorials,
this file will be named `counter.res` in your `src` directory for this example. Code
is described via inline comments:

```rescript
// This line opens the Tea.App modules into the current scope for Program access functions and types
open Tea.App

// This opens the Elm-style virtual-dom functions and types into the current scope
open Tea.Html

// Let's create a new type here to be our main message type that is passed around
type msg =
  | Increment // This will be our message to increment the counter
  | Decrement // This will be our message to decrement the counter
  | Reset     // This will be our message to reset the counter to 0
  | Set(int)  // This will be our message to set the counter to a specific value

// the model for Counter is just an integer
type model = int

// This is optional for such a simple example, but it is good to have an `init` function to define your initial model default values
let init = () => 4

// This is the central message handler, it takes the model as the first argument
let update = (model: model, msg: msg) : model =>
  switch msg {
  | Increment => model + 1
  | Decrement => model - 1
  | Reset => 0
  | Set(v) => v
  }

// This is just a helper function for the view, a simple function that returns a button based on some argument
let viewButton = (title: string, msg: msg) =
  button(list{onClick(msg)}, list{text(title)})

// This is the main callback to generate the virtual-dom.
// This returns a virtual-dom node that becomes the view, only changes from call-to-call are set on the real DOM for efficiency, this is also only called once per frame even with many messages sent in within that frame, otherwise does nothing
let view = (model: model) : Vdom.node<msg> =>
  div(
    list{},
    list{
      span(list{style("text-weight", "bold")}, list{text(string_of_int(model))}),
      br(list{}),
      viewButton("Increment", Increment),
      br(list{}),
      viewButton("Decrement", Decrement),
      br(list{}),
      viewButton("Set to 42", (Set 42)),
      br(list{}),
      model != 42 ? viewButton("Reset", Reset) : noNode
    })

// This is the main function, it can be named anything you want but `main` is
// traditional.  The Program returned here has a set of callbacks that can easily be
// called from Rescript or from javascript for running this main attached to an
// element, or even to pass a message into the event loop.  You can even expose the
// constructors to the messages to javascript via the above [@@bs.deriving
// {accessors}] attribute on the `msg` type or manually, that way even javascript can
// use it safely.
let main =
  beginnerProgram({
    model: init (),
    update: update,
    view: view
  })
```


If anything is typed wrong then the Rescript type checker will catch it and advise.

To use this from javascript (with your bundler of choice) you can just do:

```javascript
  var app = require("src/counter.res").main(document.getElementById("my-element"));
```

And if you need to shut it down or pass it a message or so then you can do so via the `app` variable, or feel free to not assign it to a variable as well.

For further examples see the [test
directory](https://github.com/darklang/rescript-tea/tree/main/test), which has many
examples.

## Starter-Kits

A list of starter-kits that get you up and running.

> Feel free to extend this list!

#### [tcoopman/bucklescript-tea-starter-kit](https://github.com/tcoopman/bucklescript-tea-starter-kit)

* syntax: OCaml
* build-system: [rollup](https://github.com/rollup/rollup)
* dev-server: [zeit/serve](https://github.com/zeit/serve)

#### [feluxe/bs-tea-starter-kit](https://github.com/feluxe/bs-tea-starter-kit)

* syntax: ReasonML
* build-system: [webpack](https://github.com/webpack/webpack)
* dev-server: [webpack-dev-server](https://github.com/webpack/webpack-dev-server)
* css: [bs-css](https://github.com/SentiaAnalytics/bs-css)

#### [darklang/philip2](https://github.com/darklang/philip2)

This one is not so much a starter kit as it is a porting kit, it can actually take in elm files, parse them, and output rescript-tea OCaml code (which can be converted to Rescript via `rescript convert`) with only minor'ish tweaks there-after needed to get it working.

See its announcement article at: https://medium.com/@paulbiggar/philip2-an-elm-to-reasonml-compiler-a210aaa6cd04

And its porting guide at: https://github.com/darklang/philip2#how-to-port-your-project

## History

Rescript-tea is a fork of
[bucklescript-tea](https://github.com/OvermindDL1/bucklescript-tea), aimed to
modernize it base on how Rescript has developed since bucklescript-tea was created. We greatly appreciate the work that [OvermindDL1](https://github.com/OvermindDL1) put into it.