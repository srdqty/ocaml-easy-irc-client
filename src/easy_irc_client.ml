open Core.Std
open Async.Std
include Message_util

type irc_writer = string Pipe.Writer.t

let send_privmsg ~target ~message writer =
  Or_error.bind
    (make_privmsg_string ~target ~message)
    (fun string -> Pipe.write_without_pushback writer string; Result.ok_unit)

let send_join ~channel writer =
  Or_error.bind 
    (make_join_string ~channel)
    (fun string -> Pipe.write_without_pushback writer string; Result.ok_unit)

let pushback writer = Pipe.pushback writer

(* Send password, send nick, send user.
 * Then check for error replies or 001
 * Password error replies:
   * ERR_NEEDMOREPARAMS
   * ERR_ALREADYREGISTERED
   *
 * Nick error replies:
   * ERR_NONICKNAMEGIVEN
   * ERR_NICKNAMEINUSE
   * ERR_ERRONEUSENICKNAME
   * ERR_NICKCOLLISION
   *
 * User error repies:
   * ERR_NEEDMOREPARAMS
   * ERR_ALREADYREGISTERED
   *
 * Success:
   * Receive "001" reply
 *)

let with_connection' 
  ~server
  ~nickname
  ?(username=nickname)
  ?(realname=nickname)
  ?password 
  ?(port=6667)
  ?(channels=[])
  ?(quit=Deferred.never())
  ~process_message
  ()
=
  let addr = Tcp.to_host_and_port server port in
  let client_reader, writer_to_client = Pipe.create () in
  let reader_from_client, client_writer = Pipe.create () in

  let (>>=) = Deferred.Or_error.Monad_infix.(>>=) in

  let get_message line = 
    match parse_message line with
      | None -> Deferred.Or_error.error_string "invalid message"
      | Some msg -> Deferred.Or_error.return msg in

  let check_response command =
    if is_welcome_message command
      then `Registered
    else if is_err_need_more_params command 
      then `Error (Or_error.error_string "ERR_NEEDMOREPARAMS")
    else if is_err_already_registered command
      then `Error (Or_error.error_string "ERR_ALREADYREGISTERED")
    else if is_err_no_nickname_given command
      then `Error (Or_error.error_string "ERR_NONICKNAMEGIVEN")
    else if is_err_nickname_in_use command
      then `Error (Or_error.error_string "ERR_NICKNAMEINUSE")
    else if is_err_erroneous_nickname command
      then `Error (Or_error.error_string "ERR_ERRONEOUSNICKNAME")
    else if is_err_nick_collision command
      then `Error (Or_error.error_string "ERR_NICKCOLLISION")
    else `DontKnow 
  in

  let can_read_from_server reader =
    let (>>=) = Deferred.(>>=) in
    Pipe.values_available reader >>= fun result ->
    match result with
      | `Ok -> Deferred.Or_error.ok_unit
      | `Eof -> Deferred.Or_error.error_string
                  "unexpected end of input from server"
  in
  let read_from_server reader =
    let (>>=) = Deferred.(>>=) in
    Pipe.read reader >>= fun result ->
    match result with
      | `Ok line -> 
          Log.Global.debug "Received from server: %s" (String.escaped line);
          Deferred.Or_error.return line
      | `Eof -> Deferred.Or_error.error_string
                  "unexpected end of input from server"
  in
  
  let is_flushed writer =
    Deferred.bind (Writer.flushed writer)
                  (fun () -> Deferred.Or_error.ok_unit) in
  let write_to_server writer line =
    let (>>=) = Deferred.Monad_infix.(>>=) in
    (* always pause 1 sec before writing to server, simple way to prevent
     * flooding *)
    after (Time.Span.of_int_sec 1) >>= fun () ->
    Log.Global.debug "Sending to server: %s" (String.escaped line);
    Writer.write writer line;
    is_flushed writer
  in

  let write_to_client msg =
    Pipe.write_without_pushback writer_to_client msg;
    Deferred.Or_error.ok_unit in

  let can_read_from_client () =
    let (>>=) = Deferred.(>>=) in
    Pipe.values_available reader_from_client >>= fun result ->
    match result with
      | `Ok  -> Deferred.Or_error.ok_unit
      | `Eof -> Deferred.Or_error.error_string
                  "unexpected end of input in reader_from_client"
  in
  let read_from_client () =
    let (>>=) = Deferred.(>>=) in
    Pipe.read reader_from_client >>= fun result ->
    match result with
      | `Ok line -> Deferred.Or_error.return line
      | `Eof -> Deferred.Or_error.error_string
                  "unexpected end of input in reader_from_client"
  in
  
  let send_pass writer = function
    | None -> Deferred.Or_error.ok_unit
    | Some password ->
        Deferred.return (make_pass_string ~password) >>= fun string ->
        write_to_server writer string in

  let send_user writer username realname = 
    Deferred.return (make_user_string ~username ~realname) >>= fun string ->
    write_to_server writer string in

  let send_nick writer nickname = 
    Deferred.return (make_nick_string ~nickname) >>= fun string ->
    write_to_server writer string in

  let send_quit writer message = 
    Deferred.return (make_quit_string ~message) >>= fun string ->
    write_to_server writer string
  in

  let register tcp_reader tcp_writer =
    send_pass tcp_writer password >>= fun () ->
    send_nick tcp_writer nickname >>= fun () ->
    send_user tcp_writer username realname >>= fun () ->
    let rec loop() =
      read_from_server tcp_reader >>= fun line ->
      get_message line >>= fun message ->
      write_to_client message >>= fun () ->
      match check_response message.command with
        | `Error error -> Deferred.return error
        | `Registered -> Deferred.Or_error.ok_unit
        | `DontKnow -> loop ()
    in
      loop ()
  in


  let join_channel channel writer =
    let (>>=) = Deferred.Or_error.Monad_infix.(>>=) in
    Deferred.return (make_join_string ~channel) >>= fun string ->
    Writer.write writer string;
    Deferred.Or_error.ok_unit
  in
  let rec join_channels writer = function
    | [] -> Deferred.Or_error.ok_unit
    | hd :: tl ->
        join_channel hd writer >>= fun () ->
        join_channels writer tl
  in

  let check_ping tcp_writer {command;arguments;_} = 
    if is_ping command
    then 
      Deferred.return (make_pong_string arguments) >>= fun string ->
      write_to_server tcp_writer string
    else Deferred.Or_error.ok_unit
  in

  let handle_receive tcp_reader tcp_writer or_error =
    Deferred.return or_error >>= fun () ->
    read_from_server tcp_reader >>= fun line ->
    get_message line >>= fun message ->
    check_ping tcp_writer message >>= fun () ->
    write_to_client message
  in

  let handle_send tcp_writer or_error =
    Deferred.return or_error >>= fun () ->
    read_from_client () >>= fun line ->
    write_to_server tcp_writer line
  in
  
  let can_pass_to_client () =
    let (>>=) = Deferred.(>>=) in
    Pipe.values_available client_reader >>= fun result ->
    match result with
      | `Ok -> Deferred.Or_error.ok_unit
      | `Eof -> Deferred.Or_error.error_string
                  "unexpected end of input in client_reader"
  in
  let pass_to_client () =
    let (>>=) = Deferred.(>>=) in
    Pipe.read client_reader >>= fun result ->
    match result with
      | `Ok msg -> Deferred.Or_error.return msg
      | `Eof -> Deferred.Or_error.error_string
                  "unexpected end of input in client_reader"
  in

  (*
  let client_write msg =
    Pipe.write_without_pushback client_writer msg;
    Deferred.Or_error.ok_unit in
*)

  let handle_callback client_writer or_error =
    Deferred.return or_error >>= fun () ->
    pass_to_client () >>= fun msg ->
    process_message client_writer msg
  in

  let main _ tcp_reader tcp_writer =
    (* let tcp_writer = Writer.pipe tcp_writer in *)
    let tcp_reader = Reader.lines tcp_reader in
    register tcp_reader tcp_writer >>= fun () ->
    join_channels tcp_writer channels >>= fun () ->
    let rec loop () =
      (* Make sure our writes to server are flushed before
       * continuing the main loop *)
      Deferred.join
       (choose
        [ 
          choice (can_pass_to_client ())
                 (fun or_error -> 
                   handle_callback client_writer or_error >>= loop)
        ; choice (can_read_from_server tcp_reader)
                 (fun or_error -> 
                   handle_receive tcp_reader tcp_writer or_error >>= loop)
        ; choice (can_read_from_client ())
                 (fun or_error ->
                   handle_send tcp_writer or_error >>= loop)
        ; choice quit
                 (fun msg -> send_quit tcp_writer msg)
        ])
    in
      loop ()
  in
    (client_writer, 
     Deferred.Or_error.try_with_join (fun () -> Tcp.with_connection addr main))

let with_connection 
  ~server
  ~nickname
  ?(username=nickname)
  ?(realname=nickname)
  ?password
  ?(port=6667) 
  ?(channels=[]) 
  ?(quit=Deferred.never())
  ~process_message
  ()
  =
  snd (with_connection'
          ~server
          ~nickname
          ~username
          ~realname
          ?password
          ~port
          ~channels
          ~quit
          ~process_message
          ())
