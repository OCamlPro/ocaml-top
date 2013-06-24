/**************************************************************************/
/*                                                                        */
/*  Copyright 2013 OCamlPro                                               */
/*                                                                        */
/*  All rights reserved.  This file is distributed under the terms of     */
/*  the GNU Public License version 3.0.                                   */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU General Public License for more details.                          */
/*                                                                        */
/**************************************************************************/

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
