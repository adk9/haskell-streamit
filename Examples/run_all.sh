#!/bin/bash

EXAMPLES="Hello File Mergesort"

# Test function
function test {
    "$@" > /dev/null
    status=$?
    if [ $status -ne 0 ]; then
	printf "\e[%sm%s\e[00m\n" 31 "[FAILED]"
    fi
    printf "\e[%sm%s\e[00m\n" 32 "[PASSED]"
}

# Cleanup function
function cleanup {
for e in $EXAMPLES
do
    pushd $e > /dev/null
    rm -f *.o *.hi ./Main;
    popd > /dev/null
done
}

trap "{ cleanup; exit; }" SIGINT SIGTERM EXIT

cleanup
for e in $EXAMPLES
do
    pushd $e > /dev/null
    printf "\e[%sm%s\e[00m\n" "4;34" "TEST: $e"
    ghc --make ./Main.hs > /dev/null
    if [ ! -x ./Main ]; then
	printf "\e[%sm%s\e[00m\n" 31 "[FAILED]"
	popd > /dev/null
	continue
    fi
    test ./Main
    popd > /dev/null
done
