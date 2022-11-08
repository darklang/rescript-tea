# 0.16.0

- New fork at https://github.com/darklang/rescript-tea
- Change library name to rescript-tea
- Switch everything to Rescript syntax
- Use rescript 9.1.4 to build
- Remove `Tea_result.result`, using builtin `result` type
- Start using [rescript-webapi](https://github.com/tinymce/rescript-webapi), and the `Dom` stdlib module, and remove almost all homegrown versions (removes almost all Web_* files)
- Replace `Tea.Html` module with existing `Tea.Html2` module
- Use `Belt.Map.String` instead of OCaml `Map.String`
- Move `Location` into `Tea_navigation`
- Convert all snake_case to camelCase
- Improve testing
- Fix spellcheck attribute
- Upstream some more Html attributes
- Remove `'` from function and type names for names that were keywords in Bucklescript but are valid in Rescript
- Re-enable CI build
- Add interface (.resi) files
- Make key parameter clearer and required