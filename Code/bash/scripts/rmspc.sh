#!/bin/sh

IFS="\n"
for file in $1;
do
    mv "$file" "${file//[[:space:]]}"
done
