-module(protocol).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-behaviour(gen_server).

%% API
-export([
	start_link/4
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  received_socket :: port(),
  received_transport :: atom(),
  ranch_transport :: atom(),
  send_socket :: port(),
  send_transport :: atom(),
  send_server :: term(),
  send_port :: term()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Ref, Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Ref, Socket, Transport, Opts}) ->
	#{
		listen_transport := ListenTransport,
		connect_server := Server,
		connect_port := Port,
		connect_transport := SendTransport
	} = Opts,
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, once}]),

	{ ok, SendSocket } = proxy1_transport_handler:connect(SendTransport, Server, Port, []),

	State = #state{
		received_socket = Socket,
		received_transport = ListenTransport,
		ranch_transport = Transport,
		send_transport = SendTransport,
		send_socket = SendSocket,
		send_server = Server,
		send_port = Port
	},
	gen_server:enter_loop(?MODULE, [], State).

handle_info(Msg, State) ->
 	#state{
 		send_transport = SendTransport,
 		send_socket = SendSocket,
 		received_transport = ReceiveTransport,
 		received_socket = ReceivedSocket
 	} = State,
	ReceiverResult = case proxy1_transport_handler:handle(ReceiveTransport, Msg) of
		{ data, Socket0, Data0} when Socket0 =:= ReceivedSocket ->
			handle_received_socket_receive_data(Data0, State);
		{ error, Socket0, _Reason0} when Socket0 =:= ReceivedSocket ->
			{ stop, State };
		{ closed, Socket0, _ } when Socket0 =:= ReceivedSocket ->
			{ stop, State };
		_ -> undefined
	end,
	SendResult = case proxy1_transport_handler:handle(SendTransport, Msg) of
		{ data, Socket1, Data1} when Socket1 =:= SendSocket ->
			handle_send_socket_received_data(Data1, State);
		{ error, Socket1, _Reason1} when Socket1 =:= SendSocket ->
			{ stop, State };
		{ closed, Socket1, _ } when Socket1 =:= SendSocket ->
			{ stop, State };
		_ -> undefined
	end,
	{ Action, NewState } = case { SendResult, ReceiverResult } of
 	  { undefined, undefined } ->
 	  	lager:info("Unknown info message ~p ~p", [ Msg, State ]),
			{ ok, State };
		{ undefined, _ } -> ReceiverResult;
 		{ _, undefined } -> SendResult;
	  { _, _ } -> error(wrong_logic, [ SendResult, ReceiverResult ])
 	end,
 	case Action of
		ok -> { noreply, NewState };
		stop -> { stop, normal, NewState }
	end.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_received_socket_receive_data(Data, State) ->
	lager:info("> ~s", [ Data ]),
	ok = proxy1_transport_handler:send(State#state.send_transport, State#state.send_socket, Data),
	{ ok, State }.

handle_send_socket_received_data(Data, State) ->
	lager:info("< ~s", [ Data ]),
	ok = proxy1_transport_handler:send(State#state.received_transport, State#state.received_socket, Data),
	{ ok, State }.