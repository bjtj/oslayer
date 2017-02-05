#!/bin/bash

set -e

BASE=$PWD
DIR_BUILD=$PWD/build
DIR_WORLD=$PWD/world

OPT=build

if [ -n "$1" ]; then
	OPT=$1
fi

clean() {
	rm -rf $DIR_BUILD $DIR_WORLD
}

reconf() {
	autoreconf -i
}

reconf_if_need() {
	if [ ! -f configure ]; then
		reconf
	fi
}

build() {
	mkdir -p $DIR_BUILD
	mkdir -p $DIR_WORLD
	cd $DIR_BUILD
	$BASE/configure --prefix="$DIR_WORLD" --enable-openssl && make && make install
}

build32() {
	mkdir -p $DIR_BUILD
	mkdir -p $DIR_WORLD
	cd $DIR_BUILD
	$BASE/configure --prefix="$DIR_WORLD" --enable-openssl --build=i686-pc-linux-gnu "CFLAGS=-m32" "CXXFLAGS=-m32" "LDFLAGS=-m32" && make && make install
}

install() {
	clean
	mkdir -p $DIR_BUILD
	cd $DIR_BUILD
	$BASE/configure --enable-openssl && make && sudo make install
}

check() {
	cd $DIR_BUILD
	make check
}

check_valgrind() {
	make -C $DIR_BUILD/tests check-valgrind-memcheck
}

case $OPT in
	reconf)
		reconf
		;;
	build)
		reconf_if_need
		build
		;;
	build32)
		reconf_if_need
		build32
		;;
	all)
		prepare
		build
		;;
	make)
		cd $DIR_BUILD && make && make install
		;;
	install)
		reconf_if_need
		install
		;;
	check)
		build
		check
		;;
	check-valgrind)
		build
		check_valgrind
		;;
	dist)
		cd $DIR_BUILD && make dist
		;;
	clean)
		clean
		;;
	*)
		echo "unknown option - $OPT"
		exit 1
		;;
esac
