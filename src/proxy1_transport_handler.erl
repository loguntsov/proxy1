-module(proxy1_transport_handler).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API
-export([
  connect/4, close/2,
  send/3, handle/2,
  ranch_transport/1,
  setopts/3
]).

-type socket() :: term().

-callback connect(Server :: string(), Port :: pos_integer(), Opts :: []) -> { ok, Socket :: socket()} | { error, Reason :: term() }.
-callback close(Socket :: socket() ) -> ok | {error, Reason :: term() }.
-callback send(Socket :: socket(), Binary :: binary()) -> ok | {error, Reason :: term() }.
-callback handle(Message :: term()) ->
  { error, Socket :: socket(), Reason :: term() } |
  { data, Socket :: socket(), Binary :: binary() } |
  { closed, Socket :: socket(), Reason :: term() } |
  unrecognized.

-callback ranch_transport() -> atom().
-callback setopts(Socket :: socket(), Opts :: term()) -> ok.

connect(Module, Server, Port, Opts) ->
  Module:connect(Server, Port, Opts).

close(Module, Socket) ->
  Module:close(Socket).

send(Module, Socket, Binary) ->
  Module:send(Socket, Binary).

handle(Module, Message) ->
  Module:handle(Message).

ranch_transport(Module) ->
  Module:ranch_transport().

setopts(Module, Socket, Opts) ->
  Module:setopts(Socket, Opts).