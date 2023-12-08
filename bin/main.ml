let parse = Ocaml_lite.Parser.parse
let typecheck = Ocaml_lite.Typechecker.type_check
let interpret = Ocaml_lite.Interpreter.interp

let () =
  if Array.length Sys.argv <> 2
  then failwith "Expected exactly one command line argument"
  else
    let ch = In_channel.open_text Sys.argv.(1) in
    let text = In_channel.input_all ch in
    let () = In_channel.close ch in
    let ast = parse text in
    let _ = typecheck ast in
    ignore (interpret ast)