#!/bin/sh
if [ -z "$1" ]
  then
    jrnl
  else
    jrnl "$@"
fi
