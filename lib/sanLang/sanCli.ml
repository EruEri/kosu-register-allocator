open Cmdliner


type cmd = {
  file: string
}
let name = "san"
let version = "0.0.1"


let file_term = 
  let info = 
    Arg.info []
    ~docv:"FILE"
  in
  Arg.required  ( Arg.pos  0 (Arg.some Arg.non_dir_file) None info) 


let cmd_term run = 
  let combine file = 
    run @@ { file }
  in
  Term.(const combine
    $ file_term
  )

let san_doc = "The minimalist 3 address code compiler"

let san_man = [
  `S Manpage.s_description;
  `P 
    "San is the compiler of a minimalist 3 address code language";
  `P
    "San exists to more easily test the register allocator of Kosu";
  `S Manpage.s_see_also;
  `P "Repository: https://github.com/EruEri/kosu-register-allocator";
  `P "Repository: https://github.com/EruEri/kosu-lang";
  `S Manpage.s_authors;
  `P "Yves Ndiaye";
  `S "COPYRIGHT";
  `P "Yves Ndiaye";
  `S "LICENSE";
  `P "San is distributed under the GNU GPL-3.0"; 
]


let san run =
  let info = Cmd.info ~doc:san_doc ~man:san_man ~version name in
  Cmd.v info (cmd_term run)

let run cmd = 
  let { file } = cmd in
  let () = ignore file in
  ()

let eval () = run |> san |> Cmd.eval ~catch:true