(*
 * Copyright (c) 2013 Richard Mortier <mort@cantab.net>
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
 *)

open Mirage

let getenv var def =
  try String.lowercase (Sys.getenv var) with Not_found -> def

let ipv4 =
(*
    let address = Ipaddr.V4.of_string_exn "46.43.42.137" in
    let netmask = Ipaddr.V4.of_string_exn "255.255.255.128" in
    let gateways = List.map Ipaddr.V4.of_string_exn ["46.43.42.129"] in
*)
  let address = Ipaddr.V4.of_string_exn "100.64.0.2" in
  let netmask = Ipaddr.V4.of_string_exn "255.255.255.128" in
  let gateways = List.map Ipaddr.V4.of_string_exn ["100.64.0.1"] in
  { address; netmask; gateways }

let port = 80

let path dir = "../store/" ^ dir

let kv_ro_of dir =
  let dir = path dir in
  let fs_mode =
    let fs = getenv "FS" "crunch" in
    match fs with
    | "fat"    -> `Fat
    | "crunch" -> `Crunch
    | fs       -> failwith ("Unknown FS mode: " ^ fs)
  in
  match fs_mode, get_mode () with
  | `Fat   , _     -> kv_ro_of_fs (fat_of_files ~dir ())
  | `Crunch, `Xen  -> crunch dir
  | `Crunch, `Unix -> direct_kv_ro dir

let server port =
  let net =
    let net = getenv "NET" "direct" in
    match net with
      | "direct" -> `Direct
      | "socket" -> `Socket
      | net      -> failwith ("Unknown NET mode: " ^ net)
  in
  let addr =
    let addr = getenv "ADDR" "dhcp" in
    match addr with
      | "static" -> `Static
      | "dhcp"   -> `Dhcp
      | "live"   -> `Live
      | addr     -> failwith ("Unknown ADDR mode: " ^ addr)
  in
  let stack console =
    match net, addr with
    | `Socket, _       -> socket_stackv4 console [Ipaddr.V4.any]
    | `Direct, `Static -> direct_stackv4_with_default_ipv4 console tap0
    | `Direct, `Dhcp   -> direct_stackv4_with_dhcp console tap0
    | `Direct, `Live   -> direct_stackv4_with_static_ipv4 console tap0 ipv4
  in
  http_server port (stack default_console)

let main =
  let packages = ["cow"; "cowabloga"] in
  let libraries = ["cow.syntax"; "cowabloga"] in
  foreign ~libraries ~packages "Server.Main"
    (console @-> http
     @-> kv_ro @-> kv_ro @-> kv_ro @-> kv_ro @-> kv_ro @-> kv_ro
     @-> job)

let () =
  register "mort-www" [
    main $ default_console $ (server port)
    $ (kv_ro_of "assets")
    $ (kv_ro_of "pages")
    $ (kv_ro_of "posts")
    $ (kv_ro_of "courses")
    (* XXX hack for now, until FAT32 supported; pr could perhaps use the mux
       combinator? *)
    $ (kv_ro_of "papers")
    $ (kv_ro_of "big-pdfs")
  ]
