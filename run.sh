#!/bin/sh

# ./run.sh <lang> <year> <day>
# <day> must be two digits

if [ $# -ne 3 ]; then
  exit 1
fi

lang/$1/run.sh $2 $3
