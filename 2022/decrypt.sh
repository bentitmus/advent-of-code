#!/bin/sh

mkdir -p input
for file in encrypted_input/*.gpg; do
  gpg --output input/$(basename $file .gpg).txt --decrypt $file
done
