open Ulib

module Make(Reader: sig
  type t
  val read : t -> int -> string  (* must read something but can be short *)
end): sig
  type t
  val make : Reader.t -> int -> t
  val getn : t -> int -> string
  val getu: t -> UChar.t
  val getu': t -> int
end = struct
  type t = Buf of int*Reader.t*Buffer.t

  let fill = function
  | Buf(s, r, b) when (Buffer.length b) = 0 ->
    let bytes = Reader.read r s in
    Buffer.add_string b bytes
  | _ -> ()

  let shift b n =
    let bytes = Buffer.sub b n ((Buffer.length b)-n) in
    Buffer.clear b;
    Buffer.add_string b bytes

  let make r size = Buf(size, r, Buffer.create size)

  let getn buf n =
    let Buf(_, _, b) = buf in
    let out = Buffer.create n in
    let rec go left =
      if left = 0 then Buffer.contents out else (
        fill buf;
        let n' = min left (Buffer.length b) in
        Buffer.add_string out (Buffer.sub b 0 n');
        shift b n';
        go (left - n')
      )
    in go n

  let getbyte buf = (getn buf 1).[0]

  let getu buf =
    let b = Buffer.create 1 in
    let rec go () =
      let byte = getbyte buf in
      Buffer.add_char b byte;
      if (int_of_char byte) > 127 then go () else ()
    in
    go ();
    UTF8.get (Buffer.contents b) 0

  let getu' buf = UChar.int_of (getu buf)
end

(* 

module ChanReader = struct
  type t = in_channel
  let read ch n =
    let bytes = String.create n in
    let n' = input ch bytes 0 n in
    String.sub bytes 0 n'
end

module ChBuf = Make(ChanReader)

let () =
  let b = ChBuf.make stdin 1024 in
  while true do
    let bytes = ChBuf.read b 2 in
    Printf.printf "%s" bytes;
    flush stdout;
  done

*)
