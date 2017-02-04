-module(proxy1_tcp_transport).

-behaviour(proxy1_transport_handler).

%% API
-export([
  connect/3, close/1,
  send/2,
  handle/1,
  ranch_transport/0,
  setopts/2
]).

connect(Server, Port, Opts) ->
  gen_tcp:connect(Server, Port, [{mode, binary}, {active, once}] ++ Opts).

close(Socket) ->
  gen_tcp:close(Socket).

send(Socket, Binary) ->
  gen_tcp:send(Socket, Binary).

handle({tcp, Socket, Data}) ->
  inet:setopts(Socket, [{active, once}]),
  { data, Socket, Data };

handle({tcp_closed, Socket}) ->
  { closed, Socket, undefined };

handle({tcp_error, Socket, Reason}) ->
  { error, Socket, Reason };

handle(_) -> unrecognized.

ranch_transport() -> ranch_tcp.

setopts(Socket, Opts) ->
  inet:setopts(Socket, Opts).

