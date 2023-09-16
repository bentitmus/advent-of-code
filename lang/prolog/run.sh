#!/bin/sh

if [ $# -ne 2 ]; then
  exit 1
fi

swipl $1/prolog/day$2.pl
