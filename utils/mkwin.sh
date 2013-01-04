#!/bin/sh -ue

PREFIX=/cygdrive/c/usr

echo "[41mDOWNLOAD[m"

# cygwin
wget http://cygwin.com/setup.exe

# iconv
wget http://sourceforge.net/projects/gnuwin32/files/libiconv/1.9.2-1/libiconv-1.9.2-1-lib.zip
wget http://sourceforge.net/projects/gnuwin32/files/libiconv/1.9.2-1/libiconv-1.9.2-1-bin.zip

# libxml2
wget ftp://ftp.zlatkovic.com/libxml/libxml2-2.7.8.win32.zip

# gtk+ bundle
wget ftp://ftp.gnome.org/pub/gnome/binaries/win32/gtk+/2.24/gtk+-bundle_2.24.10-20120208_win32.zip
wget http://ftp.acc.umu.se/pub/gnome/binaries/win32/gtksourceview/2.10/gtksourceview-dev-2.10.0.zip
wget http://ftp.acc.umu.se/pub/gnome/binaries/win32/gtksourceview/2.10/gtksourceview-2.10.0.zip

# ocaml
wget http://gallium.inria.fr/~protzenk/caml-installer/ocaml-4.00.1-i686-mingw64.exe

# lablgtk
wget http://forge.ocamlcore.org/frs/download.php/979/lablgtk-2.16.0.tar.gz


###
echo "[41mUNZIP[m"

DL=$PWD
mkdir -p "$PREFIX"
cd "$PREFIX"
unzip "$DL"/gtk+-bundle_2*.zip
unzip "$DL"/gtksourceview-2*.zip
unzip "$DL"/gtksourceview-dev-2*.zip
unzip "$DL"/libiconv-*-bin.zip
mv bin/libiconv2.dll bin/iconv.dll
unzip "$DL"/libiconv-*-lib.zip
libxml=$(ls "$DL"/libxml2-*.zip)
unzip "$libxml"
libxmldir=$(basename "$libxml" .zip)
cp -r "$libxmldir"/* .
rm -rf "$libxmldir"
mv bin/libxml2.dll bin/libxml2-2.dll

sed -i 's#^prefix=.*#prefix='"$PREFIX"'#' lib/pkgconfig/*.pc
export PKG_CONFIG_PATH="$PREFIX"/lib/pkgconfig


###
echo "[41mCOMPILE LABLGTK[m"

cd $DL
labl=$(ls lablgtk-*.tar.gz)
tar -xzf "$labl"
cd "${labl%.tar.gz}"
./configure --with-gtksourceview2 --without-glade --without-gnomecanvas
make
make opt
make install


