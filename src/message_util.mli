open Core.Std

type message = {
  prefix : string option;
  command : string;
  arguments : string list;
}

val parse_message : string -> message option

val make_pass_string : password:string -> string Or_error.t
val make_user_string : username:string -> realname:string -> string Or_error.t
val make_nick_string : nickname:string -> string Or_error.t
val make_quit_string : message:string -> string Or_error.t
val make_privmsg_string : target:string -> message:string -> string Or_error.t
val make_join_string : channel:string -> string Or_error.t

val make_pong_string : string list -> string Or_error.t

val make_message_string : message -> string Or_error.t

(* The input string is the command *)
val is_ping : string -> bool
val is_err_need_more_params : string -> bool
val is_err_already_registered : string -> bool
val is_err_no_nickname_given : string -> bool
val is_err_nickname_in_use : string -> bool
val is_err_erroneous_nickname : string -> bool
val is_err_nick_collision : string -> bool
val is_welcome_message : string -> bool
