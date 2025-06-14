open Protocol

(**/**)

module DL = Debugger_log

(**/**)

module Make (Debugger : Debugger.S) = struct
  let run { launch_args; dbg; rpc; send_stopped_events; _ } =
    let promise, resolver = Lwt.task () in
    Lwt.pause ();%lwt
    let send_initialize_event () =
      Debug_rpc.send_event rpc (module Initialized_event) ()
    in
    DL.set_rpc_command_handler rpc ~name:"Configuration-done"
      (module Configuration_done_command)
      (fun _ ->
        let open Launch_command.Arguments in
        if not launch_args.stop_on_entry then
          let stop_reason = Debugger.continue dbg in
          match stop_reason with
          | Step ->
              DL.log (fun m ->
                  m
                    "Debugger stopped because of step after running. This \
                     should not happen");
              Lwt.return_unit
          | ExecutionError ->
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(
                  make ~reason:Stopped_event.Payload.Reason.Exception
                    ~thread_id:(Some 0) ())
          | reason -> send_stopped_events reason
        else
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(
              make ~reason:Stopped_event.Payload.Reason.Entry
                ~thread_id:(Some 0) ()));
    DL.set_rpc_command_handler rpc ~name:"Disconnect"
      (module Disconnect_command)
      (fun _ ->
        DL.log (fun m -> m "Interrupting the debugger is not supported");
        Debugger.terminate dbg;
        Debug_rpc.remove_command_handler rpc (module Disconnect_command);
        Lwt.wakeup_later_exn resolver Exit;
        Lwt.return_unit);
    Lwt.join [ send_initialize_event (); promise ]
end
