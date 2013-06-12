#!/bin/bash

EXAMPLES="hello file mergesort"

# Test function
function test {
    "$@"
    status=$?
    if [ $status -ne 0 ]; then
	printf "\e[%sm%s\e[00m\n" 31 "[FAILED]"
    fi
    printf "\e[%sm%s\e[00m\n" 31 "[PASSED]"
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
    echo "Test: $e"
    ghc --make ./Main.hs > /dev/null
    if [ ! -x Main ]; then
	printf "\e[%sm%s\e[00m\n" 31 "[FAILED]"
	popd > /dev/null
	continue
    fi
    test Main
    popd > /dev/null
done
