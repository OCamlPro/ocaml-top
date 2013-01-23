#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <signal.h>

value send_sigint(value pid_val)
{
  CAMLparam1 (pid_val);

  int pid = Int_val(pid_val);
  kill(pid, 2);
  CAMLreturn (Val_unit);
}

value terminate(value pid_val)
{
  CAMLparam1 (pid_val);

  int pid = Int_val(pid_val);
  kill(pid, 9);
  CAMLreturn (Val_unit);
}
