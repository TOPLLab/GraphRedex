#!/bin/sh
# Name: Start server with tmp directory that is cleared and removed on exit
# By Robbert Gurdeep Singh
################################################################################


command -v node >/dev/null 2>&1 || { echo >&2 "I require node but it's not installed.  Aborting."; exit 1; }
command -v yarn >/dev/null 2>&1 || \
    { echo >&2 "I require yarn but it's not installed.  Aborting."; exit 1; }
command -v realpath >/dev/null 2>&1 || \
    { echo >&2 "I require realpath but it's not installed. It's most likely in the coreutils package. Aborting."; exit 1; }
curl -s -I "localhost:8529" | grep 'Server: *ArangoDB' 2>/dev/null >/dev/null ||\
    { echo >&2 "I require arangoDB to be running on port 8529 but it seems not to be up.  Aborting."; exit 1; }

run=1
dirSet=0
CURPOS=$(dirname $0)
CURLOC=$(realpath $CURPOS)
cd $CURLOC

function showHelp {
echo "$0 [options]"
cat <<HELP
  -b         to build
  -c         to clean db (implies -b)
             if -n 
  -d PATH    set directory
             if -c is set, the directory will be emptied
  -h         show this help
HELP
}

needsBuild=0
needsClean=0

while getopts "h?bcnd:" opt; do
    case "$opt" in
        h|\?)
            showHelp
            exit 0
            ;;
        b)  needsBuild=1
            ;;
        c)  needsBuild=1
            needsClean=1
            ;;
        n)  run=0
            ;;
        d)  dirSet=1
            DATADIR=$OPTARG
            ;;
        *)
            showHelp
            exit 1
    esac
done


if [[ $needsBuild -eq 1 ]]; then
    echo "Strating build"
    yarn install || \
        ( echo "could install depenencies, is yarn instlled?" ; exit 1)
    yarn run grunt || \
        ( echo "could not build the server" ; exit 1)
fi

if [[ $needsClean -eq 1 ]]; then
    echo "Strating clean"
    node setup.js || \
        ( echo "Could not clean database" ; exit 1)
fi

if [[ $dirSet -eq 1 ]]; then
    echo "Checking given directory"
    DATADIR=${DATADIR%/}
    if [[ ! -d "$DATADIR" ]]
    then
        echo "$DATADIR is not a directory"
        exit 1
    fi
    if [[ $needsClean -eq 1 ]]; then
        echo "Emptying directory $DATADIR"
        rm -r $DATADIR/*
    fi
else 
        echo "Making tmp dir"
        DATADIR=$(mktemp -d)
        trap "echo STOP;rm -rf $DATADIR" EXIT
fi



if [[ $run -eq 1 ]]
then
    node index.js "$DATADIR"
else
    echo "Not running (due -n)"
fi
