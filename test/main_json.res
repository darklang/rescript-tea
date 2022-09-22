// open Tea

// type model = string

// type msg = NewContent(string)

// let update = (_oldContent, NewContent(content)) => content

// let myStyle = {
//   open Html.Attributes
//   styles(list{("width", "100%"), ("height", "100px"), ("padding", "10px 0")})
// }

// let view = jsonString => {
//   open Html
//   let decoder = {
//     open Json.Decoder
//     keyValuePairs(string)
//   }
//   let kvPairsResult = Json.Decoder.decodeString(decoder, jsonString)
//   let reEncoded = switch kvPairsResult {
//   | Error(_) => ""
//   | Ok(v) =>
//     let encoded = {
//       open Json.Encoder
//       object_(List.map(((k, v)) => (k, string(v)), v))
//     }
//     Json.Encoder.encode(2, encoded)
//   }
//   let newEncoded = {
//     let encoded = {
//       open! Json.Encoder
//       object_(list{
//         ("string", string("a string")),
//         ("int", int(42)),
//         ("float", float(3.14)),
//         ("object", object_(list{("sub", string("object"))})),
//         ("array", array([int(1), string("2")])),
//         ("list", list(list{int(1), string("2")})),
//         ("boolean", bool(true)),
//         ("null", null),
//       })
//     }
//     Json.Encoder.encode(2, encoded)
//   }
//   let newReEncoded = {
//     let decoder = {
//       let mapper = (s, i, f, os, a0, l1, b, n) =>
//         String.concat(
//           "\n",
//           list{
//             s,
//             string_of_int(i),
//             Js.Float.toString(f),
//             os,
//             string_of_int(a0),
//             l1,
//             string_of_bool(b),
//             n,
//           },
//         )
//       open! Json.Decoder
//       map8(
//         mapper,
//         field("string", string),
//         field("int", int),
//         field("float", float),
//         at(list{"object", "sub"}, string),
//         field("array", index(0, int)),
//         field("list", index(1, string)),
//         field("boolean", bool),
//         field("null", null("null")),
//       )
//     }
//     Json.Decoder.decodeString(decoder, newEncoded)
//   }
//   let newReEncodedString = switch newReEncoded {
//   | Ok(s) => s
//   | Error(e) => "ERROR:  " ++ e
//   }
//   div(
//     list{},
//     list{
//       textarea(
//         list{
//           Tea_html.Attributes.placeholder("Put JSON string here that fullfills being an object of strings"),
//           Tea_html.Events.onInput(a => NewContent(a)),
//           myStyle,
//         },
//         list{},
//       ),
//       div(
//         list{myStyle},
//         switch kvPairsResult {
//         | Ok(v) =>
//           List.fold_left(
//             (p, (k, v)) => list{text(k ++ (": " ++ v)), br(list{}), ...p},
//             list{text("Values:")},
//             v,
//           ) |> List.rev
//         | Error(e) => list{text("ERROR: " ++ e)}
//         },
//       ),
//       div(
//         list{myStyle},
//         list{
//           text("Encoded again:"),
//           br(list{}),
//           pre(list{}, list{text(reEncoded)}),
//           br(list{}),
//           text("Encoding an object of various things to test:"),
//           br(list{}),
//           pre(list{}, list{text(newEncoded)}),
//           br(list{}),
//           text("Parsing out parts of the encoded test object from the json string"),
//           br(list{}),
//           pre(list{}, list{text(newReEncodedString)}),
//         },
//       ),
//     },
//   )
// }

// let main = {
//   open App
//   beginnerProgram({model: "", view: view, update: update})
// }
