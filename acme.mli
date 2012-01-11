exception Invalid_value
exception Short_write

type t
type event = {
  c: char * char;			(* origin, type of action *)
  q: int * int; 			(* expanded character address of action *)
  origq: int * int;		(* original character address of action *)
  flag: int;
  nu: int; 				(* num uchars in optional text *)
  text: Ulib.UTF8.t; 		(* optional text *)
  arg: Ulib.UTF8.t option;
  loc: Ulib.UTF8.t option;
}

val openwin : int -> t
val newwin : unit -> t
val read : t -> string -> string
val write : t -> string -> string -> unit
(* not supported yet due to lack of multiplexing:
	val readev : t -> event *)

val writeev : t -> event -> unit
val streamev : t -> event Event.event
