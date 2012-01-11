open Printf
open Ulib

exception Invalid_value
exception Short_write
exception Malformed_event

let mount () =
  let ns = Unix.getenv "NAMESPACE" in
  let user = Unix.getenv "USER" in
  let conn = O9pc.connect (ns^"/acme") in
  let root = O9pc.attach conn user "" in
  conn, root

let ctx = ref None

let getmount () =
  match !ctx with
  | None ->
    let m = mount () in
    ctx := Some(m);
    m
  | Some(m) ->
    m

module Fid = struct
  type t = {
    conn: O9pc.t;
    fid: int32;
    iounit: int32;
    mutable off: int64;
  }

  let openmp mount path =
    let conn, root = mount in
    let (fid, iounit) =
      O9pc.walk_open conn root false path (O9pc.oREAD lor O9pc.oWRITE) in
    {conn;fid;iounit;off=Int64.zero}

  let openp path =
    openmp (getmount ()) path

  let readfull {conn;fid;iounit} =
    let {Fcall.length=len} = O9pc.stat conn fid in
    O9pc.read conn fid iounit Int64.zero (Int64.to_int32 len)

  let writefull {conn;fid;iounit} bytes =
    let n = Int32.of_int (String.length bytes) in
    let n' = O9pc.write conn fid iounit Int64.zero n bytes in
    if n <> n' then raise Short_write

  let read f n =
    let bytes = O9pc.read f.conn f.fid f.iounit f.off (Int32.of_int n) in
    f.off <- Int64.(add f.off (of_int (String.length bytes)));
    bytes

  let write ({conn;fid;iounit} as f) bytes =
    let n = Int32.of_int (String.length bytes) in
    let n' = O9pc.write conn fid iounit f.off n bytes in
    if n <> n' then raise Short_write;
    f.off <- Int64.(add f.off (of_int32 n))

  let clunk {conn;fid} =
    O9pc.clunk conn fid
end

module BufFid = BufIO.Make(Fid)

(* this mirrors the struct in 
  http://goplan9.googlecode.com/hg/plan9/acme/acme.go *)
type event = {
  c: char * char;		(* origin, type of action *)
  q: int * int; 		(* expanded character address of action *)
  origq: int * int;	(* original character address of action *)
  flag: int;
  nu: int; 			(* num uchars in optional text *)
  text: UTF8.t; 		(* optional text *)
  arg: UTF8.t option;
  loc: UTF8.t option;
}

type t = {
  id: int;
  fids: (string, Fid.t) Hashtbl.t;
  mutable ev: BufFid.t option;
}

let mkwin id fid =
  let fids = Hashtbl.create 5 in
  Hashtbl.add fids "ctl" fid;
  {id;fids;ev=None}

let openwin id = 
  let fid = Fid.openp (sprintf "%d/ctl" id) in
  mkwin id fid

let newwin () =
  let fid = Fid.openp "new/ctl" in
  let result = Fid.readfull fid in
  match (Str.split (Str.regexp "[ \t]+") result) with
  | id :: _ -> mkwin (int_of_string id) fid
  | [] -> raise Invalid_value

let getfid w which =
  if not (Hashtbl.mem w.fids which) then
    Hashtbl.add w.fids which (Fid.openp (sprintf "%d/%s" w.id which));
  Hashtbl.find w.fids which

let read w which =
  let fid = getfid w which in
  Fid.readfull fid

let write w which bytes =
  let fid = getfid w which in
  Fid.writefull fid bytes

let closewin w =
  write w "ctl" "delete";
  Hashtbl.iter (fun _ fid -> Fid.clunk fid) w.fids

let rec readev ebuf =
  let c0 = char_of_int (geteu ebuf) in
  let c1 = char_of_int (geteu ebuf) in
  let q0 = getenum ebuf in
  let q1 = getenum ebuf in
  let flag = getenum ebuf in
  let nu = getenum ebuf in
  let rec snarf buf n =
    if n>0 then (
      UTF8.Buf.add_char buf (BufFid.getu ebuf);
      snarf buf (n-1)
    ) else UTF8.Buf.contents buf in
  let text = snarf (UTF8.Buf.create nu) nu in
  let nl = geteu ebuf in
  if (nl<>10) then raise Malformed_event;
  let c = c0, c1 in
  let q = q0 q1 in
  let ev = {c;q;origq=q;flag;nu;text;arg=None;loc=None} in
  expandchords ebuf (expandaddr ebuf ev)

and expandaddr ebuf ev =
  if (ev.flag land 2) = 0 then ev else (
    let ev' = readev ebuf in
    match ev with
    | {q=(q0, q1)} when q0=q1 ->
      {ev' with origq=ev.q;flag=ev.flag}
    | _ -> ev
  )

and expandchords ebuf ev =
  if (ev.flag land 8) = 0 then ev else (
    let ev' = readev ebuf in
    let ev'' = readev ebuf in
    {ev with arg=Some(ev'.text);loc=Some(ev''.text)}
  )

and geteu ebuf = BufFid.getu' ebuf
and geten ebuf n = BufFid.getn ebuf n

and getenum ebuf =
  let rec go n =
    let u = geteu ebuf in
    let d = u - (int_of_char '0') in
    if (d<0) or (d>9) then n else go ((n*10) + d)
  in go 0

let writeev w {c=(c0,c1);q=(q0,q1)} =
  let bytes = sprintf "%c%c%d %d \n" c0 c1 q0 q1 in
  let fid = getfid w "event" in
  Fid.write fid bytes

let rec streamev w = 
  (* We open our own connection here so that we can
     block safely: it would not be an issue if O9pc supported
     multiplexing *)
  let efid = Fid.openmp (mount ()) (sprintf "%d/event" w.id) in
  let ebuf = BufFid.make efid 256 in

  let chan = Event.new_channel () in
  let loop () =
    let open Event in
    while true do
      let ev = readev ebuf in
      sync (send chan ev)
    done
  in
  let _ = Thread.create loop () in
  Event.receive chan

let xxx () =
  let w = newwin () in
  let ev = streamev w in
  while true do
    let {c=(c0,c1);q=(q0,q1);flag;nu;text;arg;loc} as e = Event.sync ev in
    printf "c=(%c%c); q=(%d,%d); flag=%d; nu=%d; text=\"%s\"\n"
      c0 c1 q0 q1 flag nu text;
    match arg with Some(a) -> printf "	arg=%s\n" a | _ -> ();
    match loc with Some(l) -> printf "	loc=%s\n" l | _ -> ();
    flush stdout;
    if c0='M' then writeev w e;
  done
