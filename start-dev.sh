#!/bin/sh
cd `dirname $0`
exec erl +K true +A 1 -pa $PWD/ebin $PWD/deps/*/ebin -sname chaosbay -mnesia dir '"Mnesia"' -boot start_sasl -s reloader -s chaosbay $@
