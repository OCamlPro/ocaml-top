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

// #define _WIN32_WINNT 0x0501
#include <windows.h>

/* from MSDN... */

#include <strsafe.h>

void ErrorMsg(LPTSTR lpszFunction)
{
    // Retrieve the system error message for the last-error code

    LPVOID lpMsgBuf;
    LPVOID lpDisplayBuf;
    DWORD dw = GetLastError();

    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    // Display the error message and exit the process

    lpDisplayBuf = (LPVOID)LocalAlloc(LMEM_ZEROINIT,
        (lstrlen((LPCTSTR)lpMsgBuf) + lstrlen((LPCTSTR)lpszFunction) + 40) * sizeof(TCHAR));
    StringCchPrintf((LPTSTR)lpDisplayBuf,
        LocalSize(lpDisplayBuf) / sizeof(TCHAR),
        TEXT("%s failed with error %d: %s"),
        lpszFunction, dw, lpMsgBuf);
    MessageBox(NULL, (LPCTSTR)lpDisplayBuf, TEXT("Error"), MB_OK);

    LocalFree(lpMsgBuf);
    LocalFree(lpDisplayBuf);
    return;
}

/* Thanks to KindDragon from StackOverflow for this */
/* (http://stackoverflow.com/questions/813086/\
           can-i-send-a-ctrl-c-sigint-to-an-application-on-windows) */
value send_sigint(value pid_val)
{
  CAMLparam1 (pid_val);

  HANDLE pid = (HANDLE) Long_val(pid_val);

  /* for now, we are attached to the same console as the toplevel,
     which makes the following simpler; */

  // detach current console (if any)
  /* if (!FreeConsole()) */
  /*   ErrorMsg(TEXT("FreeConsole")); */

  // attach to process sg(TEXT("AttachConsole"));

  // current process should ignore ctrl_c, it will be sent to it too
  if (!SetConsoleCtrlHandler(NULL, TRUE))
    ErrorMsg(TEXT("SetConsoleCrtlHandler"));

  // generate Control+C event
  if (!GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0))
    ErrorMsg(TEXT("GenerateConsoleCtrlEvent"));

  CAMLreturn (Val_unit);
}

value terminate(value pid_val)
{
  CAMLparam1 (pid_val);

  HANDLE pid = (HANDLE) Long_val(pid_val);

  if (!TerminateProcess(pid,137))
    ErrorMsg(TEXT("TerminateProcess"));

  CAMLreturn (Val_unit);
}
