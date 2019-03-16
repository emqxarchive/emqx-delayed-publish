PROJECT = emqx_delayed_publish
PROJECT_DESCRIPTION = EMQ X Delayed Publish

NO_AUTOPATCH = cuttlefish

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

BUILD_DEPS = emqx cuttlefish

dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)

dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

EUNIT_OPTS = verbose

COVER = true

CT_SUITES = emqx_delayed_publish

CT_OPTS = -erl_args -name emqx_delayed_publish_ct@127.0.0.1

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_delayed_publish.conf -i priv/emqx_delayed_publish.schema -d data

