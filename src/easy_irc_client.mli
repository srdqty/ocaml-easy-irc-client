(** 
 This module provides a simplified client interface to interacting with
 an IRC server.
 *)

open Core.Std
open Async.Std

(** The type [message] represents a parsed IRC message *)
type message =
(*  Message_util.message = *){
  prefix : string option; (** IRC messages have an optional prefix *)
  command : string;       (** Every IRC message must have a command.
                              It will be a string of 3 digits or
                              a string of english letters. *)
  arguments : string list; (** IRC messages contain zero or more arguments.
                               The "trailing" argument will always be
                               the final argument in the list if one
                               exists. *)
}

(** The abstract type [irc_writer] is used by the functions that
    send messages to the IRC server *)
type irc_writer

(** [parse_message] attempts to parse a string as an IRC message
 
  @param string The string to parse as an IRC message
  @return Returns Some [message] if possible or None if the string
          is not a valid IRC message.
 *)
val parse_message : string -> message option

(** [send_privmsg] sends a PRIVMSG message to the IRC server
 
   @param target The target of the message, can be a channel name or a
                 nickname.
   @param message The message to send. Cannot contain ' ', '\r',
                  or '\t' characters.
   @param irc_writer The irc_writer to send the message to.
   @return If the message cannot be parsed, an error is returned.
 *)
val send_privmsg :
  target:string ->
  message:string ->
  irc_writer -> unit Or_error.t

(** [send_join] sends a JOIN message to the IRC server
 
   @param channel The IRC channel to join.
   @param irc_writer The irc_writer to send the message to.
 *)
val send_join :
  channel:string ->
  irc_writer -> unit Or_error.t

(** [pushback] returns a deferred that becomes determined
 when the irc_writer is empty or closed. This can be used to
 delay writing to prevent the write buffer from growing arbritrarily
 large.
*)
val pushback : irc_writer -> unit Deferred.t

(** 
  [with_connection] is the simplest possible way to interact
  with the IRC server. Simply write a function that will
  read the message passed to it and optional send messages back
  to the IRC server using the irc_writer parameter.

  Then pass the IRC server details and the function to
  with_connection and the library handles the rest. No need
  to worry about detecting when a connection has been
  established or keeping the connection alive.

  @param server The hostname or IP address of the IRC server
  @param nickname The desired IRC nickname
  @param username The desired IRC username
         (optional, will default to the same as nickname)
  @param realname The desired IRC real name
         (optional, will default to the same as nickname)
  @param password IRC password to send to the server.
         (optional, many servers do not require a password)
         !!!! Warning be careful this parameter, security
         of passwords has not been a focus for this library.
         !!!!
  @param port Port number for connecting the IRC server.
         (optional, defaults to 6667)
  @param channels List of channels to join once a connection
         is established.
  @param quit A deferred that should become determined when
         the connection should be terminated.
  @param process_message A function that will be called on
         each IRC message received.
*)
 
val with_connection :
  server:string ->
  nickname:string ->
  ?username:string ->
  ?realname:string ->
  ?password:string ->
  ?port:int ->
  ?channels:string list ->
  ?quit:string Deferred.t ->
  process_message:(irc_writer ->
                   message -> unit Deferred.Or_error.t) ->
  unit ->
  unit Deferred.Or_error.t

(** 
  [with_connection'] is like with_connection but it also
  returns an irc_writer so messages can be sent to the
  IRC server asynchronously. This enables client to send
  messages at any time, rather than just in response to messages received
  from the server.
*)
val with_connection' :
  server:string ->
  nickname:string ->
  ?username:string ->
  ?realname:string ->
  ?password:string ->
  ?port:int ->
  ?channels:string list ->
  ?quit:string Deferred.t ->
  process_message:(irc_writer ->
                   message -> unit Deferred.Or_error.t) ->
  unit ->
  irc_writer * unit Deferred.Or_error.t
