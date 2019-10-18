#!/bin/bash -ue

echo "Please check the file and run the commands manually,"
echo "this file is more intended like instructions."
exit 1

# apt remove flexdll (breaks stuff if installed)

MXEDIR=$PWD/../mxe
TOOLCHAIN=x86_64-w64-mingw32.static.posix

# Not certain the `.posix` part is required, but I don't want to recompile
# everything just to check


### GETTING THE C STUFF ###

git clone https://github.com/mxe/mxe.git $MXEDIR
make -C $MXEDIR MXE_TARGETS=$TOOLCHAIN gtk3 # m4 pkg-config
make -C $MXEDIR MXE_TARGETS=$TOOLCHAIN gtksourceview3 # once my contribution is merged


### GETTING THE OCAML SWITCH READY ###

opam sw cr . --repos=default,windows=git://github.com/ocaml-cross/opam-cross-windows 4.08.0

# I edited _opam/.opam-switch/switch-config to add:
cat >>_opam/.opam-switch/switch-config <<EOF
setenv: [
  [ TOOLPREF64 = "$TOOLPREF64" ]
  [ PATH := "$MXEDIR/usr/bin/" ]
]
EOF
eval $(opam env) # if you don't have the shell hooks

#opam pin ocaml-windows64 4.08.0 --edit -->
# add `["sed" "-i" "s/-warn-error A//" "stdlib/Makefile"]` first in `build:`
opam install ocaml-windows64

pin dune: needs patches 3e1ed95 and 037e1b5 in dune-configurator


### GETTING ALL THE OCAML DEPS READY ###

DEPS="ocp-indent ocp-index re cmdliner cairo2"
for d in $DEPS; do opam source $d; done

opam source lablgtk3 --dev # at 98d53ed
PATCH:
--- a/src/dune
+++ b/src/dune
@@ -139,6 +139,7 @@
  (public_name lablgtk3-sourceview3)
  (flags :standard -w -6-7-27-32-33-34-36)
  (wrapped false)
+ (no_dynlink)
  (modules_without_implementation
    gtkSourceView3_types)
  (modules


### SOME DUNE CONFIGURATION ###

cat >dune-workspace <<EOF
(lang dune 1.0)
(profile release)
(context
  (default
    (targets native windows)))
(env TOOLPREF64=$TOOLPREF64
     PATH=$PATH)
EOF

# Alternatively, just use the exported vars, and add `-x windows --profile=release` instead


### BUILD! ###

dune build --ignore-promoted-rules ocaml-top-windows.install


### RUN!!! ###

cd \\VBOXSRV\ocaml-top\_build\install\default.windows
$env:Path += ";C:\Users\IEUser\Desktop\windows-sysroot\bin"
$env:CAMLDIR += "C:\Users\IEUser\Desktop\windows-sysroot\lib\ocaml" # ?? useful ?

.\bin\ocaml-top.exe loop.ml -datadir .\share\ocaml-top\ -ocaml ocamlrun --- ocaml -I C:\Users\IEUser\Desktop\windows-sysroot\lib\ocaml


### PACKAGE ###

make windows.inst # creates a clean inst dir below windows.inst/ocaml-top
make win-inst-ocaml # bundles a minimal ocaml toplevel in the same subdir

# Get WIX
Turn on some Windows.
# (* Install .NET Framework 4.8 (https://dotnet.microsoft.com/download/thank-you/net48))
* Install WiX (https://github.com/wixtoolset/wix3/releases/download/wix3104rtm/wix310.exe) # 3.11 DOES NOT WORK
* Get the `windows.inst` directory from ocaml-top
* run CMD:
  - cd "Desktop\windows.inst"
  - .\make_msi.bat
* Enjoy the fresh `ocaml_top.msi` :D :D :D
















# # ... get WIX (wine: not working...)
# # wget https://github.com/wixtoolset/wix3/releases/download/wix3112rtm/wix311.exe
# # export WINEPREFIX=$PWD/.windows
# # export WINEARCH=win32
# # wget https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks
# # chmod a+x winetricks
# # # I have wine from debian: deb pkg wine.1.8.7-2 // wine --version = wine-2.0 (Debian 2.0-3+b2)
# # ./winetricks dotnet35
# # wine 





# ### old stuff ###


# # opam install configurator-windows --ignore-constraints-on base-windows,stdio-windows


# DEPS="ocp-indent ocp-index re cmdliner" # cairo2 ?
# for d in $DEPS; do opam source $d; done

# opam source lablgtk3 --dev # at 98d53ed

# # cat >dune-workspace.windows <<EOF
# # (lang dune 1.0)
# # (profile release)
# # (context
# #   (default
# #     (targets native windows)))
# # (env TOOLPREF64=$TOOLPREF64)

# # EOF
# # dune build --workspace dune-workspace.windows

# dune build _build/default.windows/cairo2.0.6.1/src/cairo.cmxa --profile=release --ignore-promoted-rules -x windows

# dune build ocaml-top.install --profile=release --ignore-promoted-rules


# dune build -x windows --profile=release @install
