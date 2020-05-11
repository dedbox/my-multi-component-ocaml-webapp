open Incr_dom

module Model = struct
  type t = int

  let cutoff : t -> t -> bool = ( = )

  let mk (i : int) : t = i
end

module State = struct
  type t = unit
end

module Action = struct
  open Sexplib.Conv

  type t = unit [@@deriving sexp_of]

  let apply (model : Model.t) (_action : t) () ~schedule_action:(_ : t -> unit)
      : Model.t =
    model + 1
end

let on_startup ~schedule_action:(_ : Action.t -> unit) (_ : Model.t) :
    State.t Async_kernel.Deferred.t =
  Async_kernel.return ()

let view (model : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom.Node in
  let open Vdom.Attr in
  div
    [ on_click (fun _event -> inject ()) ]
    [ text ("At " ^ string_of_int model) ]

let create (model : Model.t Incr.t) ~old_model:(_ : Model.t Incr.t)
    ~(inject : Action.t -> Vdom.Event.t) :
    (Action.t, Model.t, State.t) Component.t Incr.t =
  let%map.Incr model = model in
  let view = view model ~inject in
  Component.create model view ~apply_action:(Action.apply model)
