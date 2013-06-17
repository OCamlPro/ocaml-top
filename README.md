ocp-edit-simple
===============

A simple cross-platform OCaml code editor / IDE designed for beginners and
students.

## Features

* Full functionality on Windows
* Code edition
* Syntax coloration and indentation (in progress)
* Toplevel integration
* Compilation (todo)
* Error localisation (in progress)
* Code completion (todo)
* Easy documentation access (todo)
* Multi-file projects (later)

## Build

Builds using `ocp-build`. You should just need to run `make` or `ocp-build` from the root of the project. You will need `ocp-indent` and `ocp-index` installed too.


To build on Windows, see `utils/mkwin.sh` which may help you setup the build
environment.

## License

`Ocp-edit-simple` is released under GPLv3. See the file LICENSE for more
information. It also includes icons from the KDE oxygen set, available under
LGPLv3, and two modified files from GtkSourceView, available under LGPLv2.1 or
later.
