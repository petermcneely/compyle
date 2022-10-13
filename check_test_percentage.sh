#!/bin/sh
set -e
make clean
make tests

c=$(cat ./test_percentage.txt)
if [ $c = "100.00" ]; then
    echo "all tests passed!"
    exit 0;
else
    echo "not all tests passed :-("
    exit 1;
fi