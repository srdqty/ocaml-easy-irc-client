open Core.Std

type message =
  {
    prefix: string option;
    command: string;
    arguments: string list;
  }

let get_prefix str =
  if String.length str >= 1 && str.[0] = ':'
  then
    let (>>=) = Option.Monad_infix.(>>=) in
    String.index str ' ' >>= fun first_space ->
    Option.return (String.slice str 1 first_space, first_space)
  else None

let get_command str start_offset =
  let (>>=) = Option.Monad_infix.(>>=) in
  let rec get_command_begin_offset offset =
    if offset >= String.length str
    then None
    else if Char.is_whitespace str.[offset]
         then get_command_begin_offset (offset + 1)
         else Some offset
  in
  get_command_begin_offset start_offset >>= fun command_begin_offset ->
    let rec loop offset =
      let is_delimeter = function ' ' | '\n' | '\r' -> true | _ -> false in
      if offset > String.length str then None
      else
        let c = str.[offset] in
          if c = '\x00' then None
          else if is_delimeter c 
               then Some (String.slice str command_begin_offset offset, offset)
          else loop (offset + 1)
    in
      loop command_begin_offset

let get_trail_index str offset = String.index_from str offset ':'

(* parser still accepts message if "\r\n" is left off string *)
let check_suffix str =
  let length = String.length str in
    if length >= 2 && str.[length-1] = '\n' && str.[length-2] = '\r'
    then length - 2
    else length

let get_args str offset =
  let suffix_index = check_suffix str in
    match get_trail_index str offset with
    | None -> 
        (String.slice str offset suffix_index
          |> (String.split ~on:' ')
          |> (List.filter ~f:(fun x -> not (String.is_empty x))),
        None)
    | Some trail_index ->
        (String.slice str offset trail_index
          |> (String.split ~on:' ')
          |> (List.filter ~f:(fun x -> not (String.is_empty x))),
        Some (String.slice str (trail_index+1) suffix_index))

let get_message_body str offset =
  let (>>=) = Option.Monad_infix.(>>=) in
    get_command str offset >>= fun (command, after_command_offset) ->
    get_args str after_command_offset |> fun (args, trail) ->
    begin match trail with
      | None -> Some (command, args)
      | Some trail -> Some (command, args @ [trail])
    end

let parse_message str =
  let (>>=) = Option.Monad_infix.(>>=) in
  match get_prefix str with
  | None ->
      get_message_body str 0 >>= fun (command, arguments) ->
      Some {prefix = None; command; arguments}
  | Some (prefix, first_space) -> 
      get_message_body str first_space >>= fun (command, arguments) ->
      Some {prefix = Some prefix; command; arguments}

(* functions to check commands *)
let is_err_need_more_params str = "461" = str
let is_err_already_registered str = "462" = str
let is_err_no_nickname_given str = "431" = str
let is_err_nickname_in_use str = "433" = str
let is_err_erroneous_nickname str = "432" = str
let is_err_nick_collision str = "436" = str
let is_welcome_message str = "001" = str

let is_ping str = "PING" = str

(* Functions to make strings suitable for sending to an irc server
 * They perform some sanity checking on the inputs.
 * They all use string concatenation to build the strings
 * for the moment, might be worthwhile to convert the functions
 * to use string buffers to build up the results.
 *)

(* Checks if an string is a valid middle argument.
 * These arguments must be non-empty, cannot begin
 * with a ':' and must not contain ' ', '\r', '\n',
 * or '\x00' characters.
 *)
let check_middle ~context str =
  let is_white = function
    | ' ' | '\x00' | '\r' | '\n' -> true
    | _ -> false
  in
    if String.is_empty str
      then Or_error.errorf "%s cannot be empty" context
    else if str.[0] = ':'
      then Or_error.errorf "%s (%S) cannot begin with ':'"
                           context str
    else if String.exists str ~f:is_white
      then Or_error.errorf "%s (%S) contains illegal whitespace" 
                           context str
    else Result.ok_unit

(* Checks if an string is a valid trailing argument.
 * These arguments can be any sequence of (possibly empty)
 * characters but must not contain '\r', '\n',
 * or '\x00' characters.
 *)
let check_trailing ~context str =
  let is_white = function
    | '\x00' | '\r' | '\n' -> true
    | _ -> false
  in
    if String.exists str ~f:is_white
    then Or_error.errorf "%s (%S) contains an illegal character"
                         context str
    else Result.ok_unit

(* IRC maximum message length is 512, including the trailing \r\n. *)
let max_message_length = 512
let check_length ~context str =
  if (String.length str) > max_message_length
  then Or_error.errorf "%s (%S) is too long" context str
  else Result.ok_unit

let make_pass_string ~password =
  let (>>=) = Or_error.(>>=) in
  let context = "Password" in
  check_trailing ~context password >>= fun () ->
  let string = "PASS :" ^ password ^ "\r\n" in
  check_length ~context string >>= fun () ->
  Or_error.return string

let make_user_string ~username ~realname =
  let (>>=) = Or_error.(>>=) in
  check_middle ~context:"Username" username >>= fun () ->
  check_trailing ~context:"Real name" realname >>= fun () ->
  let string = "USER " ^ username ^ " 8 * :" ^ realname ^ "\r\n" in
  check_length ~context:"Username or real name" string >>= fun () ->
  Or_error.return string

let check_nick nickname =
  let invalid_char = function
    | '-' | '[' | ']' | '\\' | '`' | '^' | '{' | '}' -> false
    | c -> not (Char.is_alphanum c)
  in
    if String.is_empty nickname
      then Or_error.error_string "Nickname is empty"
    else if not (Char.is_alpha nickname.[0])
      then Or_error.errorf "Nickname must start with a letter: %S"
                           nickname
    else if String.exists nickname ~f:invalid_char 
      then Or_error.errorf "Nickname contains an illegal character: %S" 
                           nickname
    else Result.ok_unit
    
let make_nick_string ~nickname =
  let (>>=) = Or_error.(>>=) in
  check_nick nickname >>= fun () ->
  let string = "NICK :" ^ nickname ^ "\r\n" in
  check_length ~context:"Nickname" string >>= fun () ->
  Or_error.return string

let make_quit_string ~message =
  let (>>=) = Or_error.(>>=) in
  let context = "Quit message" in
  check_trailing ~context message >>= fun () ->
  let string = "QUIT :" ^ message ^ "\r\n" in
  check_length ~context string >>= fun () ->
  Or_error.return string

let check_channel channel =
  let invalid_char = function
    | ' ' | '\x07' | '\x00' | '\r' | '\n' | ',' -> true
    | _ -> false
  in
    if String.is_empty channel
      then Or_error.error_string "Channel is empty"
    else if not (channel.[0] = '#' || channel.[0] = '&')
      then Or_error.errorf "Channel must start with '#' or '&': %S"
                           channel
    else if String.exists channel ~f:invalid_char 
      then Or_error.errorf "Channel contains an illegal character: %S"
                           channel
    else Result.ok_unit
    
let make_join_string ~channel =
  let (>>=) = Or_error.(>>=) in
  check_channel channel >>= fun () ->
  let string = "JOIN " ^ channel ^ "\r\n" in
  check_length ~context:"Channel" string >>= fun () ->
  Or_error.return string

let make_privmsg_string ~target ~message =
  let (>>=) = Or_error.(>>=) in
  check_middle ~context:"PrivMsg target" target >>= fun () ->
  check_trailing ~context:"PrivMsg message" message >>= fun () ->
  let string = "PRIVMSG " ^ target ^ " :" ^ message ^ "\r\n" in
  check_length ~context:"PrivMsg" string >>= fun () ->
  Or_error.return string

let check_command command =
  if not (String.exists ~f:(fun x -> not (Char.is_alpha x)) command) ||
     begin
       3 = (String.length command) &&
       Char.is_digit command.[0] &&
       Char.is_digit command.[1] &&
       Char.is_digit command.[2]
     end
  then Result.ok_unit
  else Or_error.errorf "Invalid command: %S\n" command

let rec check_arguments arguments =
  let (>>=) = Or_error.Monad_infix.(>>=) in
  match arguments with 
    | [] -> Result.ok_unit
    | trail :: [] -> check_trailing ~context:"Trailing argument" trail
    | middle :: rest ->
        check_middle ~context:"Middle argument" middle >>= fun () ->
        check_arguments rest

(* Would be more efficient with a string buffer to accumulate
 * the results, but IRC messages shouldn't be to long anyway.
 * Maybe change it later
 *)
let rec args_to_string arguments =
  match arguments with
    | [] -> ""
    | trail :: [] -> " :" ^ trail
    | middle :: rest -> " " ^ middle ^ (args_to_string rest)

let make_message_string {prefix; command; arguments} =
  let (>>=) = Or_error.(>>=) in
  begin match prefix with
    | None -> Or_error.return ()
    | Some prefix ->
      check_middle ~context:"Prefix" prefix
  end >>= fun () ->
  check_command command >>= fun () ->
  check_arguments arguments >>= fun () ->
  let prefix = match prefix with None -> "" | Some s -> ":" ^ s ^ " " in
  let string = prefix ^ command ^ (args_to_string arguments) ^ "\r\n" in
  check_length ~context:"PrivMsg" string >>= fun () ->
  Or_error.return string

let make_pong_string arguments =
  let (>>=) = Or_error.(>>=) in
  check_arguments arguments >>= fun () ->
  let string = "PONG" ^ (args_to_string arguments) ^ "\r\n" in
  check_length ~context:"Pong" string >>= fun () ->
  Or_error.return string
