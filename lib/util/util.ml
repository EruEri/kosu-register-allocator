module Date = struct

  type 'a dated = {
    value: 'a;
    start: int option;
    ending: int option
  }

  let has_started {start; _} = Option.is_some start
  let has_finished {ending; _} = Option.is_some ending
  let value {value; _} = value
  let set_start debut dated = {
    dated with start = Some debut
  }
  let set_end ending dated = {
    dated with ending = Some ending
  }

  let add_date date dated = 
    match dated.start with
    | None -> { dated with start = date }
    | Some s -> set_end s dated
  
end