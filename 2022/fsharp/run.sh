#!/bin/sh

if [ $# -ne 1 ]; then
  exit 1
fi

target=day$1

dotnet fsi $target.fsx
