%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus-tcp devices on an ethernet network

-module(modbus_device).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("modbus.hrl").


%%% %%% -------------------------------------------------------------------
%% ModbusTCP gen_server API
%%% %%% -------------------------------------------------------------------

init([Host, Port, DeviceAddr]) ->
	Retval = gen_tcp:connect(Host, Port, [binary, {active,false}, {packet, 0}]),

	case Retval of
		{ok, Sock} ->
			State = #tcp_request{sock = Sock, address = DeviceAddr},
			{ok, State};
		{error,ErrorType} ->
			{stop, {error, ErrorType}}
	end.

handle_call({read_coils, Start, Offset, Opts}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_COILS,
		start = Start,
		data = Offset
	},

	{ok, Data} = send_and_receive(NewState),
	FinalData = case output(Data, Opts, coils) of
		Result when length(Result) > Offset ->
			{ResultHead, _} = lists:split(Offset, Result),
			ResultHead;
		Result -> Result
	end,
	{reply, FinalData, NewState};

handle_call({read_inputs, Start, Offset, Opts}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_INPUTS,
		start = Start,
		data = Offset
	},

	{ok, Data} = send_and_receive(NewState),
	FinalData = case output(Data, Opts, coils) of
		Result when length(Result) > Offset ->
			{ResultHead, _} = lists:split(Offset, Result),
			ResultHead;
		Result -> Result
	end,
	{reply, FinalData, NewState};


handle_call({read_hreg, Start, Offset, Opts}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_HREGS,
		start = Start,
		data = Offset
	},

	{ok, Data} = send_and_receive(NewState),
	FinalData = output(Data, Opts, int16),
	{reply, FinalData, NewState};

handle_call({read_ireg,Start, Offset, Opts}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_IREGS,
		start = Start,
		data = Offset
	},

	{ok, Data} = send_and_receive(NewState),
	FinalData = output(Data, Opts, int16),
	{reply, FinalData, NewState};

handle_call({write_hreg, Start, OrigData}, From, State) when is_integer(OrigData) ->
	handle_call({write_hreg, Start, [OrigData]}, From, State);

handle_call({write_hreg, Start, OrigData}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_WRITE_HREGS,
		start = Start,
		data = OrigData
	},

	{ok, [_Address,_FunctionCode|Data]} = send_and_receive(NewState),

	[FinalData] = modbus_util:binary_to_int16(Data),

	{reply, FinalData, NewState}.


handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, State) ->
	gen_tcp:close(State#tcp_request.sock),
	ok.

code_change(_OldVsn, State, _Extra) -> 
	 {ok, State}.


%%% %%% -------------------------------------------------------------------
%% Util
%%% %%% -------------------------------------------------------------------

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
generate_request(#tcp_request{tid = Tid, address = Address, function = ?FC_WRITE_HREGS, start = Start, data = Data}) ->
	Quantity = length(Data),
	ValuesBin = modbus_util:int16_to_binary(Data),
	ByteCount = length(binary_to_list(ValuesBin)),
	Message = <<Address:8, ?FC_WRITE_HREGS:8, Start:16, Quantity:16, ByteCount:8, ValuesBin/binary>>,

	Size = size(Message),
	<<Tid:16, 0:16, Size:16, Message/binary>>;

generate_request(#tcp_request{tid = Tid, address = Address, function = Code, start = Start, data = Offset}) ->
	Message = <<Address:8, Code:8, Start:16, Offset:16>>,
	Size = size(Message),
	<<Tid:16, 0:16, Size:16, Message/binary>>.


%% @doc Function to validate the response header and get the data from the tcp socket.
%% @end
-spec get_response(State::#tcp_request{}) -> ok | {error, term()}.
get_response(State) ->
	Tid = State#tcp_request.tid,
	Address = State#tcp_request.address,	
	Code = State#tcp_request.function,
	BadCode = Code + 128,

	case gen_tcp:recv(State#tcp_request.sock, 0) of
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
		{ok, <<Tid:16, 0:16,_TcpSize:16, Address, Code, Size, Data:Size/binary>>} ->
			{ok, Data};
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

