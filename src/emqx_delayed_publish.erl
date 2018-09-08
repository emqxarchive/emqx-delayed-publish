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

%% TODO: store the delayed publish.
-module(emqx_delayed_publish).

-behaviour(gen_server).

-include_lib("emqx/include/emqx.hrl").

-export([load/0, unload/0]).

%% Hook callbacks
-export([on_message_publish/1]).

-export([start_link/0]).

%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(APP, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Plugin callbacks
%%--------------------------------------------------------------------

load() ->
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/1, []),
    io:format("~s is loaded.~n", [?APP]), ok.

unload() ->
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/1),
    io:format("~s is unloaded.~n", [?APP]), ok.

on_message_publish(Msg = #message{topic = Topic}) ->
    do_publish(binary:split(Topic, <<"/">>, [global]), Msg).

%%--------------------------------------------------------------------
%% gen_server callback
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({delayed_publish, Msg}, State) ->
    emqx_broker:safe_publish(Msg),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

do_publish([<<"$delayed">>, DelaySec | Rem], Msg) ->
    try
        DelayedMsg = {delayed_publish, Msg#message{topic = emqx_topic:join(Rem)}},
        DelayTime = to_millisec(binary_to_integer(DelaySec)),
        erlang:send_after(DelayTime, ?MODULE, DelayedMsg),
        {stop, Msg}
    catch
        _:Reason:Statcktrace ->
            emqx_logger:error("Delayed publish error: ~p~n~p", [Reason, Statcktrace]),
            {ok, Msg}
    end;

do_publish(_NonDelayed, Msg) ->
    {ok, Msg}.

to_millisec(Sec) -> Sec * 1000.
