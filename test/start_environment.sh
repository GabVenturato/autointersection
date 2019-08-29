#/bin/bash
erl -sname environment -pa ../ebin/ -eval "environment:start(\"../conf\")."