#!/bin/sh

usage() {
  echo "Usage: $0 [example_dir]"
}

f() {
  if [ ! -f $1 ]; then
    touch ../empty
    file="../empty"
  else
    file=$1
  fi
}

lstar() {
  cd $1
  name=$(basename $1)
  f $name.logic
  logic=$file
  f $name.spec
  spec=$file
  f $name.abs
  abs=$file
  f $name.bp
  background_pred=$file
  echo ../../bin/lstar -l $logic -s $spec -a $abs -b $background_pred -f $name.bc
  ../../bin/lstar -l $logic -s $spec -a $abs -b $background_pred -f $name.bc
  cd -
}

all_examples() {
  for i in $(find . -mindepth 1 -maxdepth 1 -type d); do
    lstar $i
  done
}

case $# in
  0) all_examples;;
  1) if [ -d $1 ]; then lstar $1; else usage; fi;;
  *) usage;;
esac


