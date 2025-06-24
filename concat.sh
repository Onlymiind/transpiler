#!/bin/bash

for var in "$@"
do
    echo "$var" >> "out.txt"
    cat "$var" >> "out.txt"
done
