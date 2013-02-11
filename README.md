ocp-edit-simple
===============

A simple cross-platform OCaml code editor / IDE designed for beginners and
students.

== Features

* Full functionality on Windows
* Code edition
* Syntax coloration and indentation (in progress)
* Toplevel integration
* Compilation (todo)
* Error localisation (in progress)
* Code completion (todo)
* Easy documentation access (todo)
* Multi-file projects (later)

== Build

For now, use the lower-level Makefile in `src/`:
```
cd src
make
```

To build on Windows, see `utils/mkwin.sh` which may help you setup the build
environment.
