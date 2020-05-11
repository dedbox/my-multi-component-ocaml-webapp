open Incr_dom

module Model = struct
  type t = { a : Label.Model.t; b : Label.Model.t }

  let cutoff (model1 : t) (model2 : t) : bool =
    Label.Model.cutoff model1.a model2.a && Label.Model.cutoff model1.b model2.b

  let empty : t = { a = Label.Model.mk 0; b = Label.Model.mk 0 }
end

module State = struct
  type t = unit
end

module Action = struct
  type t = A | B [@@deriving sexp_of]

  let apply (model : Model.t) (action : t) () ~(schedule_action : t -> unit) :
      Model.t =
    let schedule_action' (ab : t) (_ : Label.Action.t) : unit =
      schedule_action ab
    in
    match action with
    | A ->
        {
          model with
          a =
            Label.Action.apply model.a () ()
              ~schedule_action:(schedule_action' A);
        }
    | B ->
        {
          model with
          b =
            Label.Action.apply model.b () ()
              ~schedule_action:(schedule_action' B);
        }
end

let on_startup ~schedule_action:(_ : Action.t -> unit) (_ : Model.t) :
    State.t Async_kernel.Deferred.t =
  Async_kernel.return ()

let view (model : Model.t) ~(inject : Action.t -> Vdom.Event.t) =
  let open Vdom.Node in
  let inject' (ab : Action.t) (_ : Label.Action.t) : Vdom.Event.t = inject ab in
  div []
    [
      Label.view model.a ~inject:(inject' A);
      br [];
      Label.view model.b ~inject:(inject' B);
    ]

let create (model : Model.t Incr.t) ~old_model:(_ : Model.t Incr.t)
    ~(inject : Action.t -> Vdom.Event.t) :
    (Action.t, Model.t, State.t) Component.t Incr.t =
  let%map.Incr model = model in
  let view = view model ~inject in
  Component.create model view ~apply_action:(Action.apply model)
