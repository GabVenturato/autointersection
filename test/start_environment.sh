#/bin/bash

ENVIRONMENT_EBIN=apps/environment/ebin/

erl -sname env -pa $ENVIRONMENT_EBIN -eval "application:start(environment)."