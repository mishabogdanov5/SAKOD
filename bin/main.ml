let () = print_endline "Hello, World!"
let create_multi n = fun x -> x * n
let () = Printf.printf "Multi 6 with func 5x: %d\n" ((create_multi 5) 6)
