#!/bin/bash

# Standard examples:
echo "====================================================================================="
echo "/!\\/!\\/!\\ Make sure the web-server is not up when running leon from the CLI /!\\/!\\/!\\"
echo "====================================================================================="
echo "Press enter to run examples..."
read a

FILES=`find oopsla-examples/{Church,List,SortedList,StrictSortedList} -name "*.scala" | sort`

for f in $FILES; do
    echo "===================================================================================="
    echo "Running on $f..."
    echo "===================================================================================="
    cat $f
    echo "Press enter to start synthesis..."
    read a
    ./leon --synthesis --cegis:gencalls --cegis:vanuatoo $f
    echo "Press enter to continue to the next problem..."
    read a
done

FILES2=`find oopsla-examples/{InsertionSort,MergeSort} -name "*.scala" | sort`


for f in $FILES2; do
    echo "===================================================================================="
    echo "Running on $f..."
    echo "===================================================================================="
    cat $f
    echo "Press enter to start synthesis..."
    read a
    ./leon --costmodel=naive --synthesis --cegis:vanuatoo --cegis:gencalls $f
    echo "Press enter to continue to the next problem..."
    read a
done
