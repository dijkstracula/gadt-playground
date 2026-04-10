(* Phantom-indexed state machine for file handles *)

module type H = sig
  (* 1. traditional typestate *)
  (* 
    type closed
    type open_r
    type open_w

    type _ handle

    val open_read : string -> open_r handle
    val open_write : string -> open_w handle
    val read_line : open_r handle -> string option * open_r handle 
    val write_line : string -> open_w handle -> open_w handle 
    (* val read_write : open_r handle -> open_w handle *)
    val close : _ handle -> closed handle 
    *)

  (* 2. polymorphic variants as phantom indices *)

  type _ handle

  val open_read : string -> [ `Read ] handle
  val open_write : string -> [ `Write ] handle
  val open_rw : string -> [ `Read | `Write ] handle
  val read_line : ([> `Read ] as 's) handle -> string option * 's handle
  val write_line : string -> ([> `Write ] as 's) handle -> 's handle
  val close : _ handle -> [ `Closed ] handle
end

module MockHandle : H = struct
  (*
    type closed
    type open_r
    type open_w *)

  type _ handle = Handle of string list ref

  let open_read _s = Handle (ref [ "line1"; "line2"; "line3" ])
  let open_write _s = Handle (ref [ "line1"; "line2"; "line3" ])
  let open_rw _s = Handle (ref [ "line1"; "line2"; "line3" ])

  let read_line (Handle lines) =
    match !lines with
    | [] -> (None, Handle lines)
    | x :: xs ->
        lines := xs;
        (Some x, Handle lines)

  let write_line s (Handle lines) =
    lines := !lines @ [ s ];
    Handle lines

  let close _ = Handle (ref [])
end
