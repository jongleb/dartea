open OUnit2

let compiler = match Sys.getenv_opt "CC" with Some cc -> cc | None -> "cc"
let header_path = "../lib/codegen/c/dartea_runtime.h"
let driver_path = "dartea_runtime_test.c"

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_file ~path content =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () ->
      output_string channel content)

let executed command =
  let channel = Unix.open_process_in (command ^ " 2>&1") in
  let output = Node_runner.read_all channel in
  let status = Unix.close_process_in channel in
  (status, String.trim output)

let exited_cleanly = function Unix.WEXITED 0 -> true | _ -> false

let suite_under ~flags =
  let directory = Filename.temp_dir "dartea_c" "" in
  let driver = Filename.concat directory driver_path in
  let binary = Filename.concat directory "dartea_runtime_test" in
  write_file
    ~path:(Filename.concat directory "dartea_runtime.h")
    (read_file header_path);
  write_file ~path:driver (read_file driver_path);
  let compilation, diagnostics =
    executed
      (Printf.sprintf "%s -std=c11 -Wall -Wextra -Werror %s -I %s -o %s %s"
         compiler flags (Filename.quote directory) (Filename.quote binary)
         (Filename.quote driver))
  in
  assert_equal ~printer:Fun.id "" diagnostics;
  assert_bool "the runtime header compiles" (exited_cleanly compilation);
  let execution, output = executed (Filename.quote binary) in
  assert_equal ~printer:Fun.id "ok" output;
  assert_bool "the runtime suite passes" (exited_cleanly execution)

let sanitized _ =
  suite_under ~flags:"-O1 -g -fsanitize=address,undefined -fno-omit-frame-pointer"

let optimized _ = suite_under ~flags:"-O2"

let suite =
  [
    "runtime under address and undefined sanitizers" >:: sanitized;
    "runtime optimized" >:: optimized;
  ]
