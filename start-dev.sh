#!/bin/sh
cd `dirname $0`
exec erl -sname chaosbay -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s chaosbay
