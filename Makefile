PROJECT      = word_collector
DEPS         = cowboy jiffy hackney jsonrec
dep_cowboy   = git https://github.com/ninenines/cowboy 1.0.1
dep_jiffy    = git https://github.com/davisp/jiffy.git 0.13.3
dep_hackney  = git https://github.com/benoitc/hackney.git 1.1.0
dep_jsonrec  = git https://github.com/EduardSergeev/jsonrec.git 0.0.3
TEST_DIR    := test
ERLC_OPTS    = $(+'{d,debug_flag}' +'{rec2json,{generate_accessors,true}}')

start:
	erl -pa ../word_collector/ebin ../word_collector/deps/*/ebin \
	-eval "word_collector_app:start()"

debug:
	erl -pa ../word_collector/ebin ../word_collector/deps/*/ebin -config elog5.config \
	-eval "word_collector_app:start()"

include erlang.mk




