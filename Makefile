PROJECT = emqx_delayed_publish
PROJECT_DESCRIPTION = EMQ X Delayed Publish
PROJECT_VERSION = 3.0

NO_AUTOPATCH = cuttlefish

BUILD_DEPS = emqx cuttlefish
dep_emqx = git https://github.com/emqtt/emqttd emqx30
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info

TEST_ERLC_OPTS += +debug_info

EUNIT_OPTS = verbose

COVER = true

CT_SUITES = emqx_delayed_publish

CT_OPTS = -erl_args -name emqx_delayed_publish_ct@127.0.0.1

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_delayed_publish.conf -i priv/emqx_delayed_publish.schema -d data

