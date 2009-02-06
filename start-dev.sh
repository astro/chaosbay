#!/bin/sh
cd `dirname $0`
exec erl +K true -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s chaosbay -name chaosbay -mnesia dir '"Mnesia"' $@
