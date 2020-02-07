%%%-------------------------------------------------------------------
%%% @author heyoka
%%% @copyright (C) 2019
%%% @doc
%%% rewrite of modbus_device as a gen_statem process with configurable reconnecting
%%% @end
%%% Created : 06. Jul 2019 10:28
%%%-------------------------------------------------------------------
-module(modbus_client).

-author("Alexander Minichmair").

-behaviour(gen_statem).


-include("modbus.hrl").
%% API
-export([start_link/0, start_link/1, start/1]).

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


-define(TCP_OPTS, [binary, {active,false}, {packet, 0}, {reuseaddr, true}, {nodelay, true}]).

-define(START_TIMEOUT, 5000).
-define(RECV_TIMEOUT, 10000).


%%-define(HANDLE_COMMON,
%%   ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

-record(state, {
   port = 8899          :: non_neg_integer(),
   host = "localhost"   :: inet:ip_address() | string(),
   device_address = 255 :: non_neg_integer(),
   socket               :: inet:socket(),
   reconnector          :: modbus_reconnector:reconnector(),
   recipient            :: pid(),
   tid = 1              :: 1..16#ff
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc start, no link
%%
%% @spec start(list()) -> {ok, Pid} | ignore | {error, Error}.
start(Opts) when is_list(Opts) ->
   gen_statem:start(?MODULE, [self(), Opts], [{timeout, ?START_TIMEOUT}]).

%%--------------------------------------------------------------------
%% @doc
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
   gen_statem:start_link(?MODULE, [self(), []], []).
start_link(Opts) when is_list(Opts) ->
   gen_statem:start_link(?MODULE, [self(), Opts], [{timeout, ?START_TIMEOUT}]).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([Recipient, Opts]) ->
   logger:set_primary_config(level, info),
   Reconnector = modbus_reconnector:new({4, 16, 5}),
   State = init_opt(Opts, #state{reconnector = Reconnector, recipient = Recipient}),
   {ok, connecting, State, [{state_timeout, 0, connect}]}.

init_opt([{host, Host}|R], State) ->
   init_opt(R, State#state{host = Host});
init_opt([{port, Port}|R], State) ->
   init_opt(R, State#state{port = Port});
init_opt([{unit_id, UnitId} | R], State) ->
   init_opt(R, State#state{device_address = UnitId});
init_opt([{min_interval, Min} | R], State) when is_integer(Min) ->
   init_opt(R, State#state{
      reconnector = modbus_reconnector:set_min_interval(State#state.reconnector, Min)});
init_opt([{max_interval, Max} | R], State) when is_integer(Max) ->
   init_opt(R, State#state{
      reconnector = modbus_reconnector:set_max_interval(State#state.reconnector, Max)});
init_opt([{max_retries, Retries} | R], State) when is_integer(Retries) orelse Retries =:= infinity ->
   init_opt(R, State#state{
      reconnector = modbus_reconnector:set_max_retries(State#state.reconnector, Retries)});
init_opt(_, State) ->
   State.


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start state functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connecting(state_timeout, connect, State) ->
   logger:info("connecting ..."),
   connect(State).

disconnected(info, {reconnect, timeout}, State) ->
   logger:info("reconnect_timeout in disconnected!"),
   connect(State);
disconnected({call, From}, _Whatever, _State) ->
   {keep_state_and_data, [{reply, From, {error, disconnected}}]};
disconnected(cast, stop, _State) ->
   {stop, normal};
disconnected(_, _, _State) ->
   keep_state_and_data.


%% connected
connected(info, {tcp_closed, _Socket}, State=#state{recipient = Rec}) ->
   Rec ! {modbus, self(), disconnected},
   try_reconnect(tcp_closed, State);
connected(info, {tcp_error, _Socket}, #state{recipient = Rec}) ->
   Rec ! {modbus, self(), tcp_error},
   keep_state_and_data;

%%%%%%%%%%%%%%%% read functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected({call, From}, {read_coils, _Start, Offset, Opts} = RO, State) ->
   NewState = next_tid(State),
   Req = read_request(NewState, RO, ?FC_READ_COILS),
   read_coils(Req, Opts, Offset, NewState, From);

connected({call, From}, {read_inputs, _Start, Offset, Opts} = RO, State) ->
   NewState = next_tid(State),
   Req = read_request(NewState, RO, ?FC_READ_INPUTS),
   read_coils(Req, Opts, Offset, NewState, From);

connected({call, From}, {read_hregs, _Start, _Offset, Opts} = RO, State) ->
   NewState = next_tid(State),
   Req = read_request(NewState, RO, ?FC_READ_HREGS),
   read_regs(Req, Opts, NewState, From);

connected({call, From}, {read_iregs, _Start, _Offset, Opts} = RO, State) ->
   NewState = next_tid(State),
   Req = read_request(NewState, RO, ?FC_READ_IREGS),
   read_regs(Req, Opts, NewState, From);

%%%%%%%%%%%% write functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected({call, From}, {write_coil, Start, Data}, State) ->
   NewState = next_tid(State),
   <<NewData:16>> = case Data of
                       0 -> <<16#0000:16>>;
                       1 -> <<16#ff00:16>>
                    end,
   Req = write_request(NewState, {write_coil, Start, NewData}, ?FC_WRITE_COIL),
   write_data(Req, NewData, NewState, From);

connected({call, From}, {write_coils, _Start, Data} = RO, State) ->
   NewState = next_tid(State),
   Req = write_request(NewState, RO, ?FC_WRITE_COILS),

   Length = if
               is_list(Data) -> length(Data);
               is_binary(Data) -> bit_size(Data)
            end,
   write_data(Req, Length, NewState, From);

connected({call, From}, {write_hreg, _Start, Data} = RO, State) ->
   NewState = next_tid(State),
   Req = write_request(NewState, RO, ?FC_WRITE_HREG),
   write_data(Req, Data, NewState, From);

connected({call, From}, {write_hregs, _Start, Data} = RO, State) ->
   NewState = next_tid(State),
   Req = write_request(NewState, RO, ?FC_WRITE_HREGS),

   Length = length(Data),
   write_data(Req, Length, NewState, From);

connected(cast, stop, _State) ->
   {stop, normal}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% end state functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terminate(_Reason, _StateName, #state{socket = undefined}) ->
   logger:notice("modbus_client shutdown ...");
terminate(_Reason, _StateName, #state{socket = Sock}) ->
   logger:notice("modbus_client disconnecting and shutdown ..."),
   catch gen_tcp:close(Sock).

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
write_request(#state{socket = Sock, tid = Tid, device_address = Address}, {_Type, Start, Data}, Function) ->
   #tcp_request{sock = Sock, tid = Tid, address = Address, start = Start, data = Data, function = Function}.

read_request(#state{socket = Sock, tid = Tid, device_address = Address}, {_Type, Start, Offset, _Opts}, Function) ->
   #tcp_request{sock = Sock, tid = Tid, address = Address, data = Offset, start = Start, function = Function}.

connect(State = #state{host = Host, port = Port, recipient = Rec}) ->
   logger:info("[Client: ~p] connecting to ~s:~p",[?MODULE, Host, Port]),
   case gen_tcp:connect(Host, Port, ?TCP_OPTS) of
      {ok, Socket} ->
         %% tell recipient we are connected
         Rec ! {modbus, self(), connected},
         {next_state, connected, State#state{socket = Socket}};
      {error, Reason} -> logger:warning("[Client: ~p] connect error to: ~p Reason: ~p" ,[?MODULE, {Host, Port}, Reason]),
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



%%%%
read_coils(Request, Opts, Offset, State, From) ->
   case send_and_receive(Request) of
      {ok, Data} -> FinalData =
                  case output(Data, Opts, coils) of
                       Result when length(Result) > Offset ->
                          {ResultHead, _} = lists:split(Offset, Result),
                          ResultHead;
                       Result -> Result
                    end,
         {keep_state, State, [{reply, From, FinalData}]};
      {error, closed} -> gen_statem:reply(From, {error, disconnected}), try_reconnect(closed, State);
      {error, Reason} -> {keep_state, State, [{reply, From, {error, Reason}}]}
   end.

read_regs(Req, Opts, State, From) ->
   case send_and_receive(Req) of
      {ok, Data} -> FinalData = output(Data, Opts, int16),
         {keep_state, State, [{reply, From, FinalData}]};
      {error, closed} -> gen_statem:reply(From, {error, disconnected}), try_reconnect(closed, State);
      {error, Reason} -> {keep_state, State, [{reply, From, {error, Reason}}]}
   end.

write_data(Req, Expected, State, From) ->
   case send_and_receive(Req) of
      {ok, Expected} -> {keep_state, State, [{reply, From, ok}]};
      {error, closed} -> gen_statem:reply(From, {error, disconnected}), try_reconnect(closed, State);
      {error, Reason} -> {keep_state, State, [{reply, From, {error, Reason}}]}
   end.

%% handle increasing transaction-id with a size of 2 bytes
next_tid(State = #state{tid = Tid}) when Tid >= 16#EFFF ->
   State#state{tid = 1};

next_tid(State = #state{tid = Tid}) ->
   State#state{tid = Tid+1}.

%% @doc Function to send the request and get the response.
%% @end
-spec send_and_receive(Request::#tcp_request{}) -> {ok, binary()}.
send_and_receive(Request) ->
   Message =  generate_request(Request),
   ok = gen_tcp:send(Request#tcp_request.sock, Message),
   get_response(Request).

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
get_response(Req = #tcp_request{}) ->
   case recv(Req) of
      {ok, Data} -> {ok, Data};
      {error, Reason} -> {error, Reason}
   end.

recv(Req) ->
   case recv(header, Req) of
      {ok, HeaderLength} ->
         recv(payload, HeaderLength, Req);
      {error,Error} ->
         {error,Error}
   end.

recv(header, #tcp_request{tid = Tid, sock = Sock}) ->
   case gen_tcp:recv(Sock, ?MBAP_LENGTH, ?RECV_TIMEOUT) of
      {ok, <<Tid:16, 0:16, Len:16>>} ->
         {ok, Len};
      {ok, Header} ->
         logger:error("Response cannot match request: request tid=~p, response header =~p", [Tid, Header]),
         {error, badresp};
      {error, Reason} ->
         {error, Reason}
   end.

recv(payload, Len, #tcp_request{sock = Sock, function = Code, start = Start}) ->
   BadCode = Code + 16#80,
   case gen_tcp:recv(Sock, Len, ?RECV_TIMEOUT) of
      {ok, <<_UnitId:8, BadCode:8, ErrorCode:8>>} -> {error, err(ErrorCode)};
      {ok, <<_UnitId:8, Code:8, Start:16, Data:16>>} -> {ok, Data};
      {ok, <<_UnitId:8, Code:8, Size, Payload:Size/binary>>} -> {ok, Payload};
      {ok, <<_:8>>}      ->   {error, too_short_modbus_payload};
      {error, Reason}    ->   {error, Reason}
   end.


%% @doc Function convert data to the selected output.
%% @end
-spec output(Data::binary(), Opts::modbus:opt_list(), Default::atom()) -> list().
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

err(<<Code:8>>) -> err(Code);
err(1)  -> illegal_function;
err(2)  -> illegal_data_address;
err(3)  -> illegal_data_value;
err(4)  -> slave_device_failure;
err(5)  -> acknowledge;
err(6)  -> slave_device_busy;
err(8)  -> memory_parity_error;
err(10) -> gateway_path_unavailable;
err(11) -> gateway_target_device_failed_to_respond;
err(_)  -> unknown_response_code.
