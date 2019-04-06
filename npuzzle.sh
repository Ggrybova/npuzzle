#!/bin/sh
#cd `dirname $0`
SHOST=`hostname -s`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin $PWD/.eunit \
    -sname 'npuzzle'@$SHOST \
    -boot start_sasl \
    -s 'npuzzle' \
