ocaml-top
=========

A simple cross-platform OCaml code editor built for top-level evaluation.

## Features

* Full functionality on Windows
* Code edition
* Syntax coloration and indentation
* Toplevel integration
* Error localisation

## Planned features

* Compilation
* Code completion
* Easy documentation access
* Multi-file projects

## Build

Builds using `ocp-build`. You should just need to run `make` or `ocp-build` from
the root of the project. You will need `ocp-indent` and `ocp-index` installed
too.

To build on Windows, you will need cygwin with mingw32, and the mingw ocaml
distribution (not cygwin !). See `utils/mkwin.sh` which may help you setup the
build environment (contains the URLs of required installers and packages, and
contains code to install the correct gtk distribution with sourceview and
prerequisite, fix pkgconfig, and compile lablgtk with gtksourceview support).
You will also need `ocp-indent` and (for completion/doc) `ocp-index`, which
should be as simple as `git clone`, `make -f Makefile.simple install`.

## License

`ocaml-top` is released under GPLv3. See the file LICENSE for more information.

It also includes icons from the KDE oxygen set, available under LGPLv3, and two
modified files from GtkSourceView, available under LGPLv2.1 or later (see the
headers in data/ for more detail).

The convenient `utils/mkwinapp` used in the windows build process is Copyright
(c) 2002-2004 by Harry Chomsky, and released under LGPL v2 or later.
