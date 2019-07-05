%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2019 10:28
%%%-------------------------------------------------------------------
-module(modbus_client).
-author("heyoka").

-behaviour(gen_statem).

-include("modbus.hrl").
%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([
   init/1,
   format_status/2,
   terminate/3,
   code_change/4,
   callback_mode/0]).

%% state.functions
-export([connecting/3, disconnected/3, connected/3]).

-define(SERVER, ?MODULE).


-define(TCP_OPTS, [binary, {active,false}, {packet, 0}]).

-define(CONN_TIMEOUT, 3000).


-record(state, {
   port = 8899           :: non_neg_integer(),
   host = "localhost"   :: inet:ip_address() | string(),
   device_address = 255 :: non_neg_integer(),
   socket               :: inet:socket(),
   reconnector          :: modbus_reconnector:reconnector(),
   recipient            :: pid(),
   tid = 1              :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
   gen_statem:start_link(?MODULE, [self()], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Recipient]) ->
   logger:set_primary_config(level, info),
   Reconnector = modbus_reconnector:new({4, 16, 5}),
   {ok, connecting, #state{reconnector = Reconnector, recipient = Recipient},
      [{state_timeout, 0, connect}]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out 
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------
callback_mode() ->
   state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
   Status = some_term,
   Status.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
connecting(state_timeout, connect, State) ->
   logger:info("connecting ..."),
   connect(State).

disconnected(info, {reconnect, timeout}, State) ->
   logger:info("reconnect_timeout in disconnected!"),
   connect(State).

%% connect
connected(info, {tcp_closed, _Socket}, State) ->
   try_reconnect(tcp_closed, State);

connected({call, From}, ModbusEvent, State) ->
   logger:notice("Unexpected call from: ~p: ~p when connected", [From, ModbusEvent]),
   {next_state, connected, State, [{reply, From, ModbusEvent}]};

connected({call, _From}, {read_coils, Start, Offset, Opts}, State) ->
   Req0 = request(State),
   Req = Req0#tcp_request{
      function = ?FC_READ_COILS,
      start = Start,
      data = Offset
   },

   {ok, Data} = send_and_receive(Req),
   FinalData = case output(Data, Opts, coils) of
                  Result when length(Result) > Offset ->
                     {ResultHead, _} = lists:split(Offset, Result),
                     ResultHead;
                  Result -> Result
               end,
   {reply, FinalData, State#state{tid = Req#tcp_request.tid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
   ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
   {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
request(#state{socket = Sock, tid = Tid, device_address = Address}) ->
   #tcp_request{sock = Sock, tid = Tid+1, address = Address}.

connect(State = #state{host = Host, port = Port, recipient = Rec}) ->
   logger:info("[Client: ~p] connecting to ~s:~p",[?MODULE, Host, Port]),
   case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
      {ok, Socket} ->
         %% tell recipient we are connected
         Rec ! {modbus, self(), connected},
         {next_state, connected, State#state{socket = Socket}};
      {error, Reason} -> logger:warning("[Client: ~p] connect error to: ~p" ,[?MODULE, {Host, Port}]),
         try_reconnect(Reason, State)
   end.

try_reconnect(Reason, State = #state{reconnector = undefined}) ->
   {stop, {shutdown, Reason}, State};
try_reconnect(Reason, State = #state{reconnector = Reconnector}) ->
   logger:info("[Client: ~p] try reconnecting...",[?MODULE]),
   case modbus_reconnector:execute(Reconnector, {reconnect, timeout}) of
      {ok, Reconnector1} ->
         {next_state, disconnected, State#state{reconnector = Reconnector1}};
      {stop, Error} -> logger:error("[Client: ~p] reconnect error: ~p!",[?MODULE, Error]),
         {stop, {shutdown, Reason}, State}
   end.


%% @doc Function to send the request and get the response.
%% @end
-spec send_and_receive(State::#tcp_request{}) -> {ok, binary()}.
send_and_receive(State) ->
   Message =  generate_request(State),
   ok = gen_tcp:send(State#tcp_request.sock, Message),
   {ok, _Data} = get_response(State).

%% @doc Function to generate  the request message from State.
%% @end
-spec generate_request(State::#tcp_request{}) -> binary().
generate_request(#tcp_request{tid = Tid, address = Address, function = ?FC_WRITE_COILS,
   start = Start, data = Data}) when is_list(Data) ->
   Length = length(Data),
   NewData = modbus_util:coils_to_binary(Data),
   ByteSize = byte_size(NewData),
   Message = <<Address:8, ?FC_WRITE_COILS:8, Start:16, Length:16, ByteSize:8, NewData/binary>>,

   Size = byte_size(Message),
   <<Tid:16, 0:16, Size:16, Message/binary>>;

generate_request(#tcp_request{tid = Tid, address = Address, function = ?FC_WRITE_COILS,
   start = Start, data = Data}) when is_binary(Data) ->
   Length = bit_size(Data),
   ByteSize = byte_size(Data),
   Message = <<Address:8, ?FC_WRITE_COILS:8, Start:16, Length:16, ByteSize:8, Data/binary>>,

   Size = byte_size(Message),
   <<Tid:16, 0:16, Size:16, Message/binary>>;

generate_request(#tcp_request{tid = Tid, address = Address, function = ?FC_WRITE_HREGS, start = Start, data = Data}) ->
   Length = length(Data),
   NewData = modbus_util:int16_to_binary(Data),
   ByteSize = byte_size(NewData),
   Message = <<Address:8, ?FC_WRITE_HREGS:8, Start:16, Length:16, ByteSize:8, NewData/binary>>,

   Size = size(Message),
   <<Tid:16, 0:16, Size:16, Message/binary>>;

generate_request(#tcp_request{tid = Tid, address = Address, function = Code, start = Start, data = Data}) ->
   Message = <<Address:8, Code:8, Start:16, Data:16>>,
   Size = size(Message),
   <<Tid:16, 0:16, Size:16, Message/binary>>.


%% @doc Function to validate the response header and get the data from the tcp socket.
%% @end
-spec get_response(State::#tcp_request{}) -> ok | {error, term()}.
get_response(#tcp_request{sock = Socket, tid = Tid, address = Address, function = Code, start = Start}) ->
   BadCode = Code + 128,

   case gen_tcp:recv(Socket, 0) of
      {ok, <<Tid:16, 0:16,_TcpSize:16, Address, BadCode, ErrorCode>>} ->
         case ErrorCode of
            1  -> {error, illegal_function};
            2  -> {error, illegal_data_address};
            3  -> {error, illegal_data_value};
            4  -> {error, slave_device_failure};
            5  -> {error, acknowledge};
            6  -> {error, slave_device_busy};
            7  -> {error, negative_ack};
            8  -> {error, memory_parity};
            10 -> {error, path_unavailable};
            11 -> {error, failed_to_response};
            _  -> {error, unknown_response_code}
         end;
      {ok, <<Tid:16, 0:16,_TcpSize:16, Address, Code, Start:16, Data:16>>} ->
         {ok, Data};
      {ok, <<Tid:16, 0:16,_TcpSize:16, Address, Code, Size, Data:Size/binary>>} ->
         {ok, Data};
      {error, closed} -> closed;
      {error, Reason} -> {error, Reason};
      Junk -> io:format("Junk: ~w~n", [Junk]), {error,junk}
end.


%% @doc Function convert data to the selected output.
%% @end
-spec output(Data::binary(), Opts::list(), Default::atom()) -> list().
output(Data, Opts, Default) ->
   Output = proplists:get_value(output, Opts, Default),
   Signed = proplists:get_value(signed, Opts, false),
   case {Output, Signed} of
      {int16, false} -> modbus_util:binary_to_int16(Data);
      {int16, true} -> modbus_util:binary_to_int16s(Data);
      {int32, false} -> modbus_util:binary_to_int32(Data);
      {int32, true} -> modbus_util:binary_to_int32s(Data);
      {float32, _} -> modbus_util:binary_to_float32(Data);
      {coils, _} -> modbus_util:binary_to_coils(Data);
      {ascii, _} -> modbus_util:binary_to_ascii(Data);
      {binary, _} -> Data;
      _ -> Data
   end.
