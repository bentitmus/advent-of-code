#!/bin/sh

latest_year=2022

session=$(<session.txt)

for year in $(seq 2015 $latest_year); do
  mkdir -p $year/input $year/encrypted_input
  for day in $(seq -w 1 25); do
    raw_file=$year/input/day$day.txt
    enc_file=$year/encrypted_input/day$day.gpg
    if [ ! -f $raw_file ]; then
      if [ -f $enc_file ]; then
        gpg --output $raw_file --decrypt $enc_file
      else
        short_day=$(echo $day | sed 's/^0//')
        curl -b session=$session https://adventofcode.com/$year/day/$short_day/input > $raw_file
      fi
    fi
    if [ ! -f $enc_file ] && [ -f $raw_file ]; then
      gpg --output $enc_file --encrypt --recipient ben.titmus@cantab.net $raw_file
    fi
  done
done
