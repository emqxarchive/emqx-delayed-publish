%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_delayed_publish_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-record(delayed_message, {key, client_id, msg}).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("emqx/include/emqx.hrl").

all() ->
    [{group, load}, 
     {group, emqx_delayed_publish}].

groups() ->
    [{load, [sequence], [load_case]}, 
     {emqx_delayed_publish, [sequence], [delayed_message_hook, 
                                         delayed_message, 
                                         cancel_publish]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqx, emqx_delayed_publish]],
    Config.

end_per_suite(_Config) ->
    [application:stop(App) || App <- [emqx_delayed_publish, emqx]].

load_case(_Config) ->
    ok = emqx_delayed_publish:unload(),
    timer:sleep(100),
    UnHooks = emqx_hooks:lookup('message.publish'),
    ?assertEqual([], UnHooks),
    ok = emqx_delayed_publish:load(),
    Hooks = emqx_hooks:lookup('message.publish'),
    ?assertEqual(1, length(Hooks)),
    ok.

delayed_message_hook(_Config) ->
    DelayedMsg = emqx_message:make(?MODULE, 1, <<"$delayed/5/publish">>, <<"delayed_m">>),
    ?assertEqual({stop, DelayedMsg#message{topic = <<"publish">>}}, on_message_publish(DelayedMsg)),

    Msg = emqx_message:make(?MODULE, 1, <<"publish">>, <<"delayed_m">>),
    ?assertEqual({ok, Msg}, on_message_publish(Msg)),

    [Key] = mnesia:dirty_all_keys(emqx_delayed_publish),
    [#delayed_message{msg = #message{payload = Payload}}] = mnesia:dirty_read({emqx_delayed_publish, Key}),
    ?assertEqual(<<"delayed_m">>, Payload),
    timer:sleep(6000),

    EmptyKey = mnesia:dirty_all_keys(emqx_delayed_publish),
    ?assertEqual([], EmptyKey).

delayed_message(_Config) ->
    DelayedMsg = emqx_message:make(?MODULE, 1, <<"publish">>, <<"delayed_m">>),
    Headers = DelayedMsg#message.headers,
    ?assertEqual(ok, emqx_delayed_publish:delay_publish(DelayedMsg#message{headers = maps:put('Will-Delay-Interval', 5, Headers)}, <<"myclient">>)),

    [Key] = mnesia:dirty_all_keys(emqx_delayed_publish),
    [#delayed_message{client_id = <<"myclient">>, msg = #message{payload = Payload}}] = mnesia:dirty_read({emqx_delayed_publish, Key}),
    ?assertEqual(<<"delayed_m">>, Payload),
    timer:sleep(6000),

    EmptyKey = mnesia:dirty_all_keys(emqx_delayed_publish),
    ?assertEqual([], EmptyKey),
    ok.

cancel_publish(_Config) ->
    DelayedMsg = emqx_message:make(?MODULE, 1, <<"publish">>, <<"delayed_m">>),
    Headers = DelayedMsg#message.headers,
    ?assertEqual(ok, emqx_delayed_publish:delay_publish(DelayedMsg#message{headers = maps:put('Will-Delay-Interval', 5, Headers)}, <<"myclient">>)),

    [Key] = mnesia:dirty_all_keys(emqx_delayed_publish),
    [#delayed_message{client_id = <<"myclient">>, msg = #message{payload = Payload}}] = mnesia:dirty_read({emqx_delayed_publish, Key}),
    ?assertEqual(<<"delayed_m">>, Payload),

    emqx_delayed_publish:cancel_publish(<<"myclient">>),
    EmptyKey = mnesia:dirty_all_keys(emqx_delayed_publish),
    ?assertEqual([], EmptyKey),
    ok.

on_message_publish(Msg) ->
    emqx_delayed_publish:on_message_publish(Msg).

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).
