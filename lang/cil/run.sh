#!/bin/sh

if [ $# -ne 2 ]; then
  exit 1
fi

target=$1-day$2
path=$1/cil/day$2.il

mkdir -p .build
ilasm -qui -pe64 -arm64 $path -out=.build/$target.exe
cp lang/cil/runtimeconfig.template.json .build/$target.runtimeconfig.json
dotnet .build/$target.exe
