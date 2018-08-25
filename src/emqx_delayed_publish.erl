%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emqx_delayed_publish).

-behaviour(gen_server).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_internal.hrl").
-export([load/0, unload/0]).

%% Hook Callbacks
-export([on_message_publish/2]).

-export([start_link/2]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(APP, ?MODULE).
-define(POOL, ?MODULE).

-record(state, {pool, id}).

%%--------------------------------------------------------------------
%% Load The Plugin
%%--------------------------------------------------------------------

load() ->
    Filters = application:get_env(?APP, filters, []),
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/2, [Filters]),
    io:format("~s is loaded.~n", [?APP]), ok.

unload() ->
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2),
    io:format("~s is unloaded.~n", [?APP]), ok.

%%--------------------------------------------------------------------
%% Delayed Publish Message
%%--------------------------------------------------------------------

on_message_publish(Msg = #mqtt_message{topic = Topic}, Filters) ->
    on_message_publish(binary:split(Topic, <<"/">>, [global]), Msg, Filters).

on_message_publish([<<"$delayed">>, DelayTime0 | Topic0], Msg, Filters) ->
    try
        Topic = emqx_topic:join(Topic0),
        case lists:filter(fun(Filter) -> emqx_topic:match(Topic, Filter) end, Filters) of
            [] ->
                {ok, Msg};
            [_] ->
                DelayTime1 = binary_to_integer(DelayTime0),
                case DelayTime1 > 0 of
                    true ->
                        delayed_publish(Topic, Msg#mqtt_message{topic = Topic}, DelayTime1 + erlang:system_time(seconds)),
                        {stop, Msg};
                    false ->
                        {stop, Msg}
                end
        end
    catch
        _:Reason->
            lager:error("Delayed publish reason ~p, error:~p", [Reason, erlang:get_stacktrace()]),
            {ok, Msg}
    end;

on_message_publish(_, Msg, _Filters) ->
    {ok, Msg}.

start_link(Pool, Id) ->
    gen_server:start_link({local, ?PROC_NAME(?MODULE, Id)}, ?MODULE, [Pool, Id], []).

delayed_publish(Topic, Msg, DelayTime) ->
    Pid = gproc_pool:pick_worker(?POOL, Topic),
    gen_server:cast(Pid, {delayed_publish, Topic, Msg, DelayTime}).

%%--------------------------------------------------------------------
%% gen_server callback
%%--------------------------------------------------------------------
init([Pool, Id]) ->
    ?GPROC_POOL(join, Pool, Id),
    {ok, #state{pool = Pool, id = Id}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({delayed_publish, Topic, Msg, DelayTime}, State) ->
    Interval = (DelayTime*1000) - erlang:system_time(milli_seconds),
    erlang:send_after(Interval, self(), {release_publish, Topic, Msg}),
    {noreply, State, hibernate}.

handle_info({release_publish, Topic, Msg}, State) ->
    emqx_pubsub:publish(Topic, Msg),
    {noreply, State, hibernate};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pool = Pool, id = Id}) ->
    ?GPROC_POOL(leave, Pool, Id).

code_change(_, State, _) ->
    {ok, State}.

