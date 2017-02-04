-module(proxy1_ssl_transport).

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
  ssl:connect(Server, Port, [{ mode, binary}, {active, once}] ++ Opts).

close(Socket) ->
  ssl:close(Socket).

send(Socket, Binary) ->
  ssl:send(Socket, Binary).

handle({ssl, Socket, Data}) ->
  ssl:setopts(Socket, [{active, once}]),
  { data, Socket, Data };

handle({ssl_closed, Socket}) ->
  { closed, Socket, undefined };

handle({ssl_error, Socket, Reason}) ->
  { error, Socket, Reason };

handle(_) -> unrecognized.

ranch_transport() -> ranch_ssl.

setopts(Socket, Opts) ->
  ssl:setopts(Socket, Opts).
