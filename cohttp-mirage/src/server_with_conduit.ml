include Make.Server(Conduit_mirage_flow)

let connect
  : type cfg t flow.
     (cfg, t, flow) Conduit_mirage.Service.t
  -> (cfg -> _ -> unit Lwt.t) Lwt.t
  = fun service ->
    let server = fun cfg spec ->
      let handler (flow : Conduit_mirage.flow) = listen spec flow in
      let _, run = Conduit_mirage.serve service ~handler cfg in
      run in
    Lwt.return server
