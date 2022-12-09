#!/bin/sh

for file in input/*.txt; do
  gpg --output encrypted_input/$(basename $file .txt).gpg --encrypt --recipient ben.titmus@cantab.net $file
done
