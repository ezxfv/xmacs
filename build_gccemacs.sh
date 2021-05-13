#!/bin/sh
# native-comp optimization
mkdir ~/.local/emacs
cd ~/.local/emacs
git clone --depth 1 -b feature/native-comp https://github.com/emacs-mirror/emacs.git gccemacs || true

GCC_VERSION="10.2.0_4"

export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:${PATH}"
export CFLAGS="-I/usr/local/Cellar/gcc/${GCC_VERSION}/include -O2 -march=native"
export LDFLAGS="-L/usr/local/Cellar/gcc/${GCC_VERSION}/lib/gcc/10 -I/usr/local/Cellar/gcc/${GCC_VERSION}/include"
export LIBRARY_PATH="/usr/local/Cellar/gcc/${GCC_VERSION}/lib/gcc/10:/usr/local/opt/libgccjit/lib/gcc/10:${LIBRARY_PATH:-}"
cd gccemacs  || exit

git clean -xfd

echo ""
echo "autogen"
echo ""
./autogen.sh

echo ""
echo "configure"
echo ""
export CC=/usr/bin/clang
./configure \
     --disable-silent-rules \
     --enable-locallisppath=/usr/local/share/emacs/28.0.50/site-lisp \
     --prefix=/usr/local/opt/gccemacs \
     --without-dbus \
     --without-imagemagick \
     --with-mailutils \
     --with-ns \
     --with-json \
     --disable-ns-self-contained \
     --with-cairo \
     --with-modules \
     --with-xml2 \
     --with-gnutls \
     --with-rsvg \
     --with-native-compilation

read -p "Press any key to resume ..."

# Ensure /usr/local/opt/gccemacs exists
rm -rf /usr/local/opt/gccemacs
mkdir /usr/local/opt/gccemacs

# Ensure the directory to which we will dump Emacs exists and has the correct
# permissions set.
libexec=/usr/local/libexec/emacs/28.0.50
if [ ! -d $libexec ]; then
  sudo mkdir -p $libexec
  sudo chown $USER $libexec
fi

echo ""
echo "make"
echo ""
export CC=/usr/local/bin/gcc-10
export CXX=/usr/local/bin/g++-10

case $OSTYPE in
darwin*)
    NCPU=$(sysctl -n hw.logicalcpu)
    ;;
*)
    NCPU=$(nproc)
    ;;
esac

make -j${NCPU} NATIVE_FULL_AOT=1
sudo make install

rm -rf "/Applications/Gccemacs.app"
mv nextstep/Emacs.app "/Applications/Gccemacs.app"

rm -rf /usr/local/native-lisp/
sudo cp -R native-lisp /usr/local

cd /usr/local/bin || exit
rm gccemacs
rm gccemacsclient
ln -s /usr/local/opt/gccemacs/bin/emacs ./gccemacs
ln -s /usr/local/opt/gccemacs/bin/emacsclient ./gccemacsclient


cd /Applications/Gccemacs.app/Contents || exit
ln -s /usr/local/opt/gccemacs/share/emacs/28.0.50/lisp .
