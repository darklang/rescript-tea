let first: (result<'msg, 'b>, result<'c, 'b>) => result<'msg, 'b>

let resultToOption: result<'msg, 'b> => option<'msg>

let errorOfFirst: (result<'a, 'b>, result<'c, 'b>) => option<'b>
