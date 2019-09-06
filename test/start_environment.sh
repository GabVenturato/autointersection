#/bin/bash
erl -sname env -pa ebin/ -eval "application:start(environment)."