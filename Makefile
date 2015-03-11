PROJECT = word_collector
DEPS = cowboy jiffy hackney
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_jiffy = git https://github.com/davisp/jiffy.git 0.13.3
dep_hackney = git https://github.com/benoitc/hackney.git 1.1.0
TEST_DIR ?= test
include erlang.mk




