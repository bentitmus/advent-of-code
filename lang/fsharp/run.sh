#!/bin/sh

if [ $# -ne 2 ]; then
  exit 1
fi

dotnet fsi $1/fsharp/day$2.fsx
