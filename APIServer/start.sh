#!/bin/sh
# Name: Start server with tmp directory that is cleared and removed on exit
# By Robbert Gurdeep Singh
################################################################################

run=1
dirSet=0

while getopts "h?bcnd:" opt; do
    case "$opt" in
        h|\?)
            echo "-b to build"
            echo "-d to set directory"
            echo "-c to clean db"
            echo "-n to not run"
            exit 0
            ;;
        b)  yarn install && yarn run grunt
            ;;
        c)  (yarn install && \
            yarn run grunt && \
            node setup.js) || ( echo "could not clear DB" ; exit 1)
            echo "ensure that the datadir is emptied"
            ;;
        n)  run=0
            ;;
        d)  dirSet=1
            MYTMPDIR=$OPTARG
            ;;
    esac
done

if [[ $run -eq 1 ]]
then
    if [[ $dirSet -eq 0 ]]
    then
        echo "Making tmp dir"
        MYTMPDIR=$(mktemp -d)
        trap "echo STOP;rm -rf $MYTMPDIR" EXIT
    fi

    MYTMPDIR=${MYTMPDIR%/}
    if [[ -d "$MYTMPDIR" ]]
    then
        node index.js "$MYTMPDIR"
    else
        echo "$MYTMPDIR is not a directory"
    fi

else
    echo "Not running"
fi
