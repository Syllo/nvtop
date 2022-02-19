#!/bin/bash
doc="Usage: $0 prefix\n\tprefix: Path to directory you want to install nvtop and ncurses, default is \$HOME/bin"

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

echo $path

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


