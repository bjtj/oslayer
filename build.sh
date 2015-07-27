#!/bin/bash

BASE=$PWD
DIR_BUILD=$PWD/build
DIR_WORLD=$PWD/world

OPT=build

if [ -n "$1" ]; then
	OPT=$1
fi

prepare() {
	autoreconf -i
	mkdir -p $DIR_BUILD
	mkdir -p $DIR_WORLD
}

build() {
	cd $DIR_BUILD
	$BASE/configure --prefix "$DIR_WORLD" && make && make install
}

case $OPT in
	prepare)
		prepare
		;;
	build)
		build
		;;
	all)
		prepare
		build
		;;
	make)
		cd $DIR_BUILD && make && make install
		;;
	dist)
		cd $DIR_BUILD && make dist
		;;
	clean)
		rm -rf $DIR_BUILD $DIR_WORLD
		;;
	*)
		echo "unknown option - $OPT"
		exit 1
		;;
esac
