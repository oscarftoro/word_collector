PROJECT      = word_collector
DEPS         = cowboy jiffy hackney sync xref_runner
dep_cowboy   = git https://github.com/ninenines/cowboy 1.0.1
dep_jiffy    = git https://github.com/davisp/jiffy.git 0.13.3
dep_hackney  = git https://github.com/benoitc/hackney.git 1.1.0
dep_sync     = git https://github.com/rustyio/sync.git
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2
TEST_DIR          := test

include erlang.mk

DEBUG_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +debug_info

shell: app
	erl -pa ../word_collector/ebin ../word_collector/deps/*/ebin \
	-eval "word_collector_app:start()"

debug: ERLC_OPTS = -D debug_flag
debug: clean app 
	erl -pa ../word_collector/ebin ../word_collector/deps/*/ebin -config elog5.config \
	-eval "word_collector_app:start()"




