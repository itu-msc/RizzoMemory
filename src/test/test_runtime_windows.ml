let compile_and_run_c ~source =
  let source_file = Filename.temp_file "windows-runtime" ".c" in
  let binary_file = Filename.temp_file "windows-runtime" ".exe" in
  let original_cwd = Sys.getcwd () in
  let remove_if_exists path =
    if Sys.file_exists path then Sys.remove path
  in
  let write_source () =
    let out_chan = open_out source_file in
    Fun.protect
      ~finally:(fun () -> close_out out_chan)
      (fun () -> output_string out_chan source)
  in
  try
    write_source ();
    Fun.protect
      ~finally:(fun () ->
        Sys.chdir original_cwd;
        remove_if_exists source_file;
        remove_if_exists binary_file)
      (fun () ->
        Sys.chdir "../../../..";
        let command =
          Rizzoc.to_shell_command
            (Rizzoc.generated_c_compiler_invocation ~input_file:source_file ~output_file:binary_file ())
        in
        let status = Sys.command command in
        if status <> 0
        then Alcotest.failf "C compile failed with status %d. Command: %s" status command;
        let exit_code = Sys.command (Filename.quote binary_file) in
        Alcotest.(check int) "C helper exits successfully" 0 exit_code)
  with exn ->
    remove_if_exists source_file;
    remove_if_exists binary_file;
    Alcotest.failf "Windows runtime helper test failed: %s" (Printexc.to_string exn)

let test_console_timeout_reader_waits_for_completed_line () =
  if not Sys.win32 then ()
  else
    compile_and_run_c
      ~source:
        {|#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <windows.h>

#include "os.h"

static int fail_at(int line) {
    fprintf(stderr, "failure at line %d\n", line);
    return line;
}

int main(void) {
    char buffer[8];
    rz_os_result_t result = RZ_TIMEOUT;
    bool done = false;
    KEY_EVENT_RECORD key_event;
    size_t index;

    rz_console_line_reset();
    ZeroMemory(&key_event, sizeof(key_event));
    key_event.bKeyDown = TRUE;
    key_event.uChar.AsciiChar = 'a';
    done = rz_console_handle_key_event(&key_event, buffer, sizeof(buffer), &result);
    if (done) {
        return fail_at(__LINE__);
    }
    if (rz_console_line_state.length != 1 || rz_console_line_state.buffer[0] != 'a') {
        return fail_at(__LINE__);
    }

    ZeroMemory(&key_event, sizeof(key_event));
    key_event.bKeyDown = TRUE;
    key_event.uChar.AsciiChar = '\r';
    done = rz_console_handle_key_event(&key_event, buffer, sizeof(buffer), &result);
    if (!done || result != RZ_OK || strcmp(buffer, "a") != 0) {
        return fail_at(__LINE__);
    }

    rz_console_line_reset();
    for (index = 0; index < 9; index++) {
        ZeroMemory(&key_event, sizeof(key_event));
        key_event.bKeyDown = TRUE;
        key_event.uChar.AsciiChar = 'x';
        done = rz_console_handle_key_event(&key_event, buffer, sizeof(buffer), &result);
        if (done) {
            return fail_at(__LINE__);
        }
    }

    ZeroMemory(&key_event, sizeof(key_event));
    key_event.bKeyDown = TRUE;
    key_event.uChar.AsciiChar = '\r';
    done = rz_console_handle_key_event(&key_event, buffer, sizeof(buffer), &result);
    if (!done || result != RZ_INPUT_TOO_LONG) {
        return fail_at(__LINE__);
    }

    return 0;
}
|}

let windows_runtime_tests = [
  "Windows console timeout reader waits for completed line", `Quick,
  test_console_timeout_reader_waits_for_completed_line;
]