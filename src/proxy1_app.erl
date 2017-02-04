-module(proxy1_app).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(application).

%% Application callbacks
-export([
  main/0,
  start/2,
  stop/1
]).

-export([
  env/1
]).

-define(APP, proxy1).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

main() ->
  { ok, _ } = application:ensure_all_started(proxy1),
  ok.

start(_StartType, _StartArgs) ->

  ListenTransport = env(listen_transport),
  ListenPort = env(listen_port),

  {ok, _} = ranch:start_listener(proxy, 1, proxy1_transport_handler:ranch_transport(ListenTransport), [{port, ListenPort}], protocol, #{
    listen_transport => ListenTransport,
    connect_server => env(connect_server),
    connect_port => env(connect_port),
    connect_transport => env(connect_transport)
  }),

  { ok, Pid } = proxy1_app_sup:start_link(),
  { ok, Pid }.

stop(_State) ->
  ok.

env(Key) ->
  { ok, Value } = application:get_env(?APP, Key),
  Value.

%%%===================================================================
%%% Internal functions
%%%===================================================================
