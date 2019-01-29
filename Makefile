PROJECT = emqx_delayed_publish
PROJECT_DESCRIPTION = EMQ X Delayed Publish
PROJECT_VERSION = 3.1

NO_AUTOPATCH = cuttlefish

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx release-3.1
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

