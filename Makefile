PROJECT = emqx_delayed_publish
PROJECT_DESCRIPTION = EMQ X Delayed Publish
PROJECT_VERSION = 2.4

NO_AUTOPATCH = cuttlefish

DEPS = jsx
dep_jsx = git https://github.com/talentdeficit/jsx

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqx/emqx-enterprise
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

EUNIT_OPTS = verbose

COVER = true

CT_SUITES = emqx_delayed_publish

CT_OPTS = -erl_args -name emqx_delayed_publish_ct@127.0.0.1

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_delayed_publish.conf -i priv/emqx_delayed_publish.schema -d data

