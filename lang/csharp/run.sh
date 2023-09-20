#!/bin/sh

if [ $# -ne 2 ]; then
  exit 1
fi

dotnet script $1/csharp/day$2.csx
