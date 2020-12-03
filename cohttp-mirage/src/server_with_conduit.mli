
(** HTTP server with conduit. *)
include Cohttp_lwt.S.Server with type IO.conn = Conduit_mirage.flow

val connect :
  ('cfg, 't, 'flow) Conduit_mirage.Service.t ->
  ('cfg -> t -> unit Lwt.t) Lwt.t
