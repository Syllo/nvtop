#!/bin/bash
doc="Usage: $0 prefix\n\tprefix: Path to directory you want to install nvtop and ncurses, default is \$HOME/bin"
reminder="Remember to add $path to PATH to your shell configuration file. For example,\n (1) echo export PATH=$path:\$PATH >> ~/.bashrc, if you are using bash.\n (2) echo export PATH=$path:\$PATH >> ~/.zshrc, if you are using zsh.\nYou could either reopen the shell afterwards, or simple use\n\tsource ~/\$PATH_TO_SH_CONFIG"
# test if -h or --help is present
for i in "$@"; do
    if [[ $i == "-h" || $i == "--help" ]]; then
        echo -e $doc
        exit 1
    fi
done

path=$HOME/bin
home=`pwd`

if [ $# -gt 0 ]; then
    path=$1
fi

if [ -f $path ]; then
    echo $path exists. Please specify another path.
    exit
fi

mkdir -p $path

if [ ! -d $path/ncurses ]; then
    cd ./ncurses-6.3
    ./configure --prefix=$path/ncurses
    make && make install
    ln -s $path/ncurses/include/ncurses/ncurses.h $path/ncurses/include/ncurses.h
fi

cd $home
mkdir build && cd build
cmake -D CURSES_LIBRARY=$path/ncurses/lib/libncurses.a -D CURSES_INCLUDE_PATH=$path/ncurses/include ..
make 
make DESTDIR=$path install

mv $path/usr/local/bin/nvtop $path
echo -e $reminder


