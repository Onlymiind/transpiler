#!bin/bash
echo $1 >> "out.txt"
echo "\n" >> "out.txt"
cat $1 >> "out.txt"
echo "\n" >> "out.txt"
