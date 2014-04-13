#!/usr/bin/env sh
ERL_LIBS=deps erl -pa ebin -s ssldump proxy $@
