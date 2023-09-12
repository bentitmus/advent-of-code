#!/bin/sh

if [ $# -ne 1 ]; then
  exit 1
fi

target=day$1

mkdir -p .build
ilasm -qui -pe64 -arm64 $target.il -out=.build/$target.exe
cp runtimeconfig.template.json .build/$target.runtimeconfig.json
dotnet .build/$target.exe
