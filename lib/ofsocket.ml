(*
 * Copyright (c) 2012 Haris Rotsos <cr409@cl.cam.ac.uk>
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
 *)
open Openflow_net_lwt
open Lwt
exception ReadError

let sp = Printf.sprintf
let pp = Printf.printf
let ep = Printf.eprintf
let cp = pp "%s\n%!"

type t = {
  sock: Channel.t;
  data_cache: Cstruct.buf list ref; 
}

let create_socket sock = 
  { sock; data_cache=(ref []);}

let rec read_data t len = 
(*     Printf.printf "let rec read_data t data_cache %d = \n%!" len;  *)
    match (len, !(t.data_cache)) with
    | (0, _) -> 
(*        pp "| (0, _) ->\n%!";  *)
      return (Cstruct.sub (Lwt_bytes.create 10) 0 0 )
    | (_, []) ->
(*        pp " | (_, []) ->\n%!";  *)
      lwt data = Channel.read_some t.sock in
        t.data_cache := [data];
        read_data t len
    | (_, head::tail) 
        when ((List.fold_right (fun a b ->b+(Cstruct.len a)) tail (Cstruct.len head))>=len) -> (
(*           pp "| (_, head::tail) when ((List.fold_right (f a b
 *           ->b+(Cstruct.len b)) tail (Cstruct.len head)) >= len) ->\n%!"; *)
          let ret = Lwt_bytes.create 4096 in 
          let ret_len = ref 0 in 
          let rec read_data_inner = function 
            | head::tail when ((!ret_len + (Cstruct.len head)) < len) ->
                let _ = Cstruct.blit_buffer head 0 ret !ret_len (Cstruct.len head) in
                  ret_len := !ret_len + (Cstruct.len head);
                  read_data_inner tail
            | head::tail when ((!ret_len + (Cstruct.len head)) = len) -> 
                let _ = Cstruct.blit_buffer head 0 ret !ret_len (Cstruct.len head) in
                  ret_len := !ret_len + (Cstruct.len head);
                  t.data_cache := tail;
                  return ()
            | head::tail when ((!ret_len + (Cstruct.len head)) > len) -> 
                let len_rest = len - !ret_len in 
                let _ = Cstruct.blit_buffer head 0 ret !ret_len len_rest in
                let head = Cstruct.shift head len_rest in 
                  ret_len := !ret_len + len_rest;
                  t.data_cache := [head] @ tail; 
                  return ()
            | rest -> 
                pp "read_data:Invalid state(req_len:%d,read_len:%d,avail_data:%d)\n%!"
                  len !ret_len (List.fold_right (fun a b -> b + (Cstruct.len a)) rest 0);
                raise ReadError
          in 
          lwt _ = read_data_inner !(t.data_cache) in
          let ret = Cstruct.sub_buffer ret 0 len in  
            return (ret)
        )
    | (_, head::tail) 
        when ((List.fold_right (fun a b ->b+(Cstruct.len a)) tail (Cstruct.len head)) < len) -> (
(*        pp "| (_, head::tail) when ((List.fold_right (f a b ->b+(Cstruct.len
 *        b)) tail (Cstruct.len head)) < len) ->\n%!"; *)
      lwt data = Channel.read_some t.sock in
        t.data_cache := !(t.data_cache) @ [data];
        read_data t len)
    | (_, _) ->
(*        pp "| (_, _) ->\n%!";  *)
      Printf.printf "read_data and not match found\n%!";
      return (Cstruct.sub (Lwt_bytes.create 10) 0 0 )
