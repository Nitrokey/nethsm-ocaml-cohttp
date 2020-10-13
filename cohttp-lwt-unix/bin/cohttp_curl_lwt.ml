(*{{{ Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Lwt
open Cohttp
open Cohttp_lwt_unix

let src = Logs.Src.create "cohttp.lwt.curl" ~doc:"Cohttp Lwt curl implementation"
module Log = (val Logs.src_log src : Logs.LOG)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Mirage_crypto_rng_unix.initialize ()

let client uri ofile meth' ssl =
  Log.debug (fun d -> d "Client with URI %s" (Uri.to_string uri));
  let meth = Cohttp.Code.method_of_string meth' in
  Log.debug (fun d -> d "Client %s issued" meth');
  let ctx = match Uri.scheme uri, ssl with
    | Some "https", true ->
      let () = Ssl.init () in
      let port = Option.value ~default:443 (Uri.port uri) in
      let context = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
      let verify ctx flow =
          let socket = Conduit_lwt.TCP.Protocol.file_descr flow in
          let lwt_ssl_socket = Lwt_ssl.embed_uninitialized_socket socket ctx in
          let ssl_socket = Lwt_ssl.ssl_socket_of_uninitialized_socket lwt_ssl_socket in
          ( match Uri.host uri with
          | Some host ->
            Ssl.set_hostflags ssl_socket [Ssl.No_wildcards] ;
            Ssl.set_host ssl_socket host ;
            Lwt_ssl.ssl_perform_handshake lwt_ssl_socket >|= fun v -> Ok v
          | _ -> Lwt_ssl.ssl_connect socket ctx >|= fun v -> Ok v) in
      Conduit_lwt.add ~priority:0
        Conduit_lwt_ssl.TCP.protocol
        (Conduit_lwt_ssl.TCP.resolve ~verify ~port ~context) Conduit.empty
    | _ -> Conduit.empty in
  Client.call ~ctx meth uri >>= fun (resp, body) ->
  let status = Response.status resp in
  Log.debug (fun d ->
    d "Client %s returned: %s" meth' (Code.string_of_status status)
  );
  (* TODO follow redirects *)
  match Code.is_success (Code.code_of_status status) with
  | false ->
    prerr_endline (Code.string_of_status status);
    exit 1
  | true ->
    Cohttp_lwt.Body.length body >>= fun (len, body) ->
    Log.debug (fun d -> d "Client body length: %Ld" len);
    Cohttp_lwt.Body.to_string body >>= fun _s ->
    let output_body c =
      Lwt_stream.iter_s (Lwt_io.fprint c) (Cohttp_lwt.Body.to_stream body) in
    match ofile with
    | None -> output_body Lwt_io.stdout
    | Some fname -> Lwt_io.with_file ~mode:Lwt_io.output fname output_body

let run_client verbose ofile uri meth ssl =
  Lwt_main.run (
    (if verbose
    then (
      (* activate debug sets the reporter *)
      Cohttp_lwt_unix.Debug.activate_debug ();
      Logs.set_reporter (reporter Fmt.stderr) ;
      Logs.set_level ~all:true (Some Logs.Debug) ;
      Log.debug (fun d -> d ">>> Debug active");
      return ())
    else return ())
    >>= fun () ->
    client uri ofile meth ssl
  )

open Cmdliner

let uri =
  let loc : Uri.t Arg.converter =
    let parse s =
      try `Ok (Uri.of_string s)
      with Failure _ -> `Error "unable to parse URI" in
    parse, fun ppf p -> Format.fprintf ppf "%s" (Uri.to_string p)
  in
  Arg.(required & pos 0 (some loc) None & info [] ~docv:"URI"
   ~doc:"string of the remote address (e.g. https://google.com)")

let ssl =
  let doc = "With SSL" in
  Arg.(value & flag & info [ "with-ssl" ] ~doc)

let meth =
  let doc = "Set http method" in
  Arg.(value & opt string "GET" & info ["X"; "request"] ~doc)

let verb =
  let doc = "Display additional debugging to standard error." in
  Arg.(value & flag & info ["v"; "verbose"]  ~doc)

let ofile =
  let doc = "Output filename to store the URI into." in
  Arg.(value & opt (some string) None & info ["o"] ~docv:"FILE" ~doc)

let cmd =
  let doc = "retrieve a remote URI contents" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) fetches the remote $(i,URI) and prints it to standard output. \
        The output file can also be specified with the $(b,-o) option, and \
        more verbose debugging out obtained via the $(b,-v) option.";
    `S "BUGS";
    `P "Report them via e-mail to <mirageos-devel@lists.xenproject.org>, or \
        on the issue tracker at <https://github.com/mirage/ocaml-cohttp/issues>";
    `S "SEE ALSO";
    `P "$(b,curl)(1), $(b,wget)(1)" ]
  in
  Term.(pure run_client $ verb $ ofile $ uri $ meth $ ssl),
  Term.info "cohttp-curl" ~version:Cohttp.Conf.version ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0
