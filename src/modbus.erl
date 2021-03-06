%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus-tcp devices on an ethernet network

-module(modbus).
-export([
	connect/3, connect/1, connect_link/3, connect_link/1,
	disconnect/1,
	read_coils/3,
	read_inputs/3,
	read_iregs/3,
	read_hregs/3,
	read_coils/4,
	read_inputs/4,
	read_iregs/4,
	read_hregs/4,
	read_memory/2,
	read_memory/3,
	write_coil/3,
	write_coils/3,
	write_hreg/3,
	write_hregs/3,
	write_memory/2,
	write_memory/3]).


-export_type([opt_list/0]).
-type opt_list() :: [{signed, true|false} | {output, int16|int32|float32|coils|acii|binary}].

%%% %%% -------------------------------------------------------------------
%% Basic Modbus functions
%%% %%% -------------------------------------------------------------------

%% @doc Functions to connect with the modbus device.
%% @end
-spec connect(Host::string(), Port::integer(), DeviceAddr::integer()) -> {ok, pid()} | {error, term()}.
connect(Host, Port, DeviceAddr) ->
	connect([{host, Host}, {port, Port}, {unit_id, DeviceAddr}]).

connect(Opts) when is_list(Opts) ->
	modbus_client:start(Opts).

connect_link(Host, Port, DeviceAddr) ->
	connect_link([{host, Host}, {port, Port}, {unit_id, DeviceAddr}]).

connect_link(Opts) when is_list(Opts) ->
	modbus_client:start_link(Opts).

%% @doc Function to disconnect the modbus device.
%% @end
-spec disconnect(Pid::pid()) -> ok.
disconnect(Pid) ->
	gen_statem:cast(Pid, stop).

%% @doc Function to request coils from the modbus device.
%% @end
-spec read_coils(Pid::pid(), Start::integer(), Offset::integer()) -> [0|1].
read_coils(Pid, Start, Offset) ->
	read_coils(Pid, Start, Offset, []).

%% @doc Function to request coils from the modbus device.
%% @end
-spec read_coils(Pid::pid(), Start::integer(), Offset::integer(), Opts::opt_list()) -> [0|1].
read_coils(Pid, Start, Offset, Opts) ->
	gen_statem:call(Pid, {read_coils, Start, Offset, Opts}).

%% @doc Function to request inputs from the modbus device.
%% @end
-spec read_inputs(Pid::pid(), Start::integer(), Offset::integer()) -> [0|1].
read_inputs(Pid, Start, Offset) ->
	read_inputs(Pid, Start, Offset, []).

%% @doc Function to request inputs from the modbus device.
%% @end
-spec read_inputs(Pid::pid(), Start::integer(), Offset::integer(), Opts::opt_list()) -> [0|1].
read_inputs(Pid, Start, Offset, Opts) ->
	gen_statem:call(Pid, {read_inputs, Start, Offset, Opts}).

%% @doc Function to request holding registers from the modbus device.
%% @end
-spec read_hregs(Pid::pid(), Start::integer(), Offset::integer()) -> [integer()].
read_hregs(Pid, Start, Offset) ->
	read_hregs(Pid, Start, Offset, []).

%% @doc Function to request holding registers from the modbus device.
%% @end
-spec read_hregs(Pid::pid(), Start::integer(), Offset::integer(), Opts::opt_list()) ->[integer()].
read_hregs(Pid, Start, Offset, Opts) ->
	gen_statem:call(Pid, {read_hregs, Start, Offset, Opts}).

%% @doc Function to request input registers from the modbus device.
%% @end
-spec read_iregs(Pid::pid(), Start::integer(), Offset::integer()) ->[integer()].
read_iregs(Pid, Start, Offset) ->
	read_iregs(Pid, Start, Offset, []).

%% @doc Function to request input registers from the modbus device.
%% @end
-spec read_iregs(Pid::pid(), Start::integer(), Offset::integer(), Opts::opt_list()) ->[integer()].
read_iregs(Pid, Start, Offset, Opts) ->
	gen_statem:call(Pid, {read_iregs, Start, Offset, Opts}).

%% @doc Function to write data on a single coil from the modbus device.
%% @end
-spec write_coil(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_coil(Pid, Start, Value) ->
	gen_statem:call(Pid, {write_coil, Start, Value}).

%% @doc Function to write data on multiple coils from the modbus device.
%% @end
-spec write_coils(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_coils(Pid, Start, Value) ->
	gen_statem:call(Pid, {write_coils, Start, Value}).

%% @doc Function to write data on a single register from the modbus device.
%% @end
-spec write_hreg(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_hreg(Pid, Start, Value) ->
	gen_statem:call(Pid, {write_hreg, Start, Value}).

%% @doc Function to write data on multiple registers from the modbus device.
%% @end
-spec write_hregs(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_hregs(Pid, Start, Value) ->
	gen_statem:call(Pid, {write_hregs, Start, Value}).


%%% %%% -------------------------------------------------------------------
%% Extended functions
%%% %%% -------------------------------------------------------------------

%% @doc Function to request a memory position from the modbus device.
%% @end
-spec read_memory(Pid::pid(), string(), Offset::integer()) -> number() | [number()].
read_memory(Pid, "%MD0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum) *32,
	gen_statem:call(Pid, {read_coils, Reg, Offset*32, [{output, float32}]});

read_memory(Pid, "%MW0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum) *16,
	gen_statem:call(Pid, {read_coils, Reg, Offset*16, [{output, int16}]});

read_memory(Pid, "%MB0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum) *8,
	gen_statem:call(Pid, {read_coils, Reg, Offset*8, [{output, ascii}]});

read_memory(Pid, "%MX0." ++ PosNum, Offset) ->
	[Word, Bit] = string:tokens(PosNum, "."),
	Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
	gen_statem:call(Pid, {read_coils, Reg, Offset, [{output, coils}]}).


%% @doc Function to request a list of memory positions from the modbus device.
%% @end
-spec read_memory(Pid::pid(), list()) -> [{string(), number()}].
read_memory(Pid, "%M" ++ _ = MemPosition) ->
	read_memory(Pid, MemPosition, 1);

read_memory(Pid, List) ->
	NewList  = lists:foldl( fun(Elem, Acc) ->
		case Elem of
			"%MD0." ++ PosNum ->
				Reg = erlang:list_to_integer(PosNum) *32,
				[{Reg, Elem} | Acc];
			"%MW0." ++ PosNum ->
				Reg = erlang:list_to_integer(PosNum) *16,
				[{Reg, Elem} | Acc];
			"%MB0." ++ PosNum ->
				Reg = erlang:list_to_integer(PosNum) *8,
				[{Reg, Elem} | Acc];
			"%MX0." ++ PosNum ->
				[Word, Bit] = string:tokens(PosNum, "."),
				Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
				[{Reg, Elem} | Acc]
		end
	end, [], List),

	ReqList = lists:foldl( fun(Elem, [{RegAcc, OffsetAcc, MemAcc} |Acc]) ->
		NewRegAcc = RegAcc + OffsetAcc,
		case Elem of
			{NewRegAcc, "%MD0." ++ _ = MemPosition} ->
				[{RegAcc, OffsetAcc +32, [MemPosition |MemAcc]} | Acc];
			{NewRegAcc, "%MW0." ++ _ = MemPosition} ->
				[{RegAcc, OffsetAcc +16, [MemPosition |MemAcc]} | Acc];
			{NewRegAcc, "%MB0." ++ _ = MemPosition} ->
				[{RegAcc, OffsetAcc +8, [MemPosition |MemAcc]} | Acc];
			{NewRegAcc, "%MX0." ++ _ = MemPosition} ->
				case MemAcc of
					[["%MX0." ++_ | _] = MxAcc |MemAccTail] when length(MxAcc) < 8 ->
						[{RegAcc, OffsetAcc +1, [[MemPosition |MxAcc] |MemAccTail]} | Acc];
					_ ->
						[{RegAcc, OffsetAcc +1, [[MemPosition] |MemAcc]} | Acc]
				end;
			{Reg, "%MD0." ++ _ = MemPosition} ->
				[{Reg, 32, [MemPosition]}, {RegAcc, OffsetAcc, MemAcc} | Acc];
			{Reg, "%MW0." ++ _ = MemPosition} ->
				[{Reg, 16, [MemPosition]}, {RegAcc, OffsetAcc, MemAcc} | Acc];
			{Reg, "%MB0." ++ _ = MemPosition} ->
				[{Reg, 8, [MemPosition]}, {RegAcc, OffsetAcc, MemAcc} | Acc];
			{Reg, "%MX0." ++ _ = MemPosition} ->
				[{Reg, 1, [[MemPosition]]}, {RegAcc, OffsetAcc, MemAcc} | Acc]
		end
	end, [{0, 0, []}], lists:usort(NewList)),

	lists:foldl( fun(Elem, Acc) ->
		case Elem of 
			{0, 0, []} ->
				Acc;
			{Reg, Offset, MemList} ->
				Data = gen_statem:call(Pid, {read_coils, Reg, Offset, [{output, binary}]}),
				{ResultList, _} = lists:foldl( fun(Mem, {MemAcc, DataAcc}) ->
					case Mem of
						"%MD0." ++ _ ->
							<<Result:32/float, DataTail/binary>> = DataAcc,
							{[{Mem, Result} |MemAcc], DataTail};
						"%MW0." ++ _ ->
							<<Result:16/integer, DataTail/binary>> = DataAcc,
							{[{Mem, Result} |MemAcc], DataTail};
						"%MB0." ++ _ ->
							<<Result:8/integer, DataTail/binary>> = DataAcc,
							{[{Mem, Result} |MemAcc], DataTail};
						["%MX0." ++ _ |_] = MxList ->
							<<Result:8/integer, DataTail/binary>> = DataAcc,
							MxResultList = lists:reverse(erlang:integer_to_list(Result, 2)),
							{FinalMxList, _} = lists:foldl( fun(MxElem, {MxAcc, MxResultAcc}) ->
								case MxResultAcc of
									[MxH | MxT] ->
										{[{MxElem, erlang:list_to_integer([MxH])} |MxAcc], MxT};
									[] ->
										{[{MxElem, 0} |MxAcc], []}
								end
							end, {[], MxResultList}, lists:reverse(MxList)),
							{MemAcc ++ FinalMxList, DataTail}
					end
				end, {Acc, Data}, lists:reverse(MemList)),
				ResultList
		end
	end, [], ReqList).


%% @doc Function to write a memory position from the modbus device.
%% @end
-spec write_memory(Pid::pid(), string(), Data::term()) -> number() | [number()].
write_memory(Pid, "%MD0." ++ PosNum, Data) ->
	Reg = erlang:list_to_integer(PosNum) *32,
	gen_statem:call(Pid, {write_coils, Reg, <<Data:32/float>>});

write_memory(Pid, "%MW0." ++ PosNum, Data) ->
	Reg = erlang:list_to_integer(PosNum) *16,
	gen_statem:call(Pid, {write_coils, Reg, <<Data:16/integer>>});

write_memory(Pid, "%MB0." ++ PosNum, Data) ->
	Reg = erlang:list_to_integer(PosNum) *8,
	gen_statem:call(Pid, {write_coils, Reg, <<Data:8/integer>>});

write_memory(Pid, "%MX0." ++ PosNum, Data) ->
	[Word, Bit] = string:tokens(PosNum, "."),
	Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
	gen_statem:call(Pid, {write_coil, Reg, Data}).


%% @doc Function to request a list of memory positions from the modbus device.
%% @end
-spec write_memory(Pid::pid(), list()) -> [{string(), number()}].
write_memory(Pid, List) ->
	NewList  = lists:foldl( fun(Elem, Acc) ->
		case Elem of
			{"%MD0." ++ PosNum = Mem, Value} ->
				Reg = erlang:list_to_integer(PosNum) *32,
				[{Reg, Mem, Value} | Acc];
			{"%MW0." ++ PosNum = Mem, Value} ->
				Reg = erlang:list_to_integer(PosNum) *16,
				[{Reg, Mem, Value} | Acc];
			{"%MB0." ++ PosNum = Mem, Value} ->
				Reg = erlang:list_to_integer(PosNum) *8,
				[{Reg, Mem, Value} | Acc];
			{"%MX0." ++ PosNum = Mem, Value} ->
				[Word, Bit] = string:tokens(PosNum, "."),
				Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
				[{Reg, Mem, Value} | Acc]
		end
	end, [], List),

	ReqList = lists:foldl( fun(Elem, [{RegAcc, OffsetAcc, BinAcc} |Acc]) ->
		NewRegAcc = RegAcc + OffsetAcc,
		case Elem of
			{NewRegAcc, "%MD0." ++ _, Value} ->
				case BinAcc of
					[_|_] ->
						[{NewRegAcc, 32, <<Value:32/float>>}, {RegAcc, OffsetAcc, BinAcc} | Acc];
					<<_/binary>> ->
						[{RegAcc, OffsetAcc +32, <<BinAcc/binary, Value:32/float>>} | Acc]
				end;
			{NewRegAcc, "%MW0." ++ _, Value} ->
				case BinAcc of
					[_|_] ->
						[{NewRegAcc, 16, <<Value:16/integer>>}, {RegAcc, OffsetAcc, BinAcc} | Acc];
					<<_/binary>> ->
						[{RegAcc, OffsetAcc +16, <<BinAcc/binary, Value:16/integer>>} | Acc]
				end;
			{NewRegAcc, "%MB0." ++ _, Value} ->
				case BinAcc of
					[_|_] ->
						[{NewRegAcc, 8, <<Value:8/integer>>}, {RegAcc, OffsetAcc, BinAcc} | Acc];
					<<_/binary>> ->
						[{RegAcc, OffsetAcc +8, <<BinAcc/binary, Value:8/integer>>} | Acc]
				end;
			{NewRegAcc, "%MX0." ++ _, Value} ->
				case BinAcc of
					[_|_] ->
						[{RegAcc, OffsetAcc +1, BinAcc ++ [Value]} | Acc];
					<<_/binary>> ->
						[{NewRegAcc, 1, [Value]}, {RegAcc, OffsetAcc, BinAcc} | Acc]
				end;
			{Reg, "%MD0." ++ _, Value} ->
				[{Reg, 32, <<Value:32/float>>}, {RegAcc, OffsetAcc, BinAcc} | Acc];
			{Reg, "%MW0." ++ _, Value} ->
				[{Reg, 16, <<Value:16/integer>>}, {RegAcc, OffsetAcc, BinAcc} | Acc];
			{Reg, "%MB0." ++ _, Value} ->
				[{Reg, 8, <<Value:8/integer>>}, {RegAcc, OffsetAcc, BinAcc} | Acc];
			{Reg, "%MX0." ++ _, Value} ->
				[{Reg, 1, [Value]}, {RegAcc, OffsetAcc, BinAcc} | Acc]
		end
	end, [{0, 0, <<>>}], lists:usort(NewList)),

	lists:map( fun(Elem) ->
		case Elem of 
			{0, 0, <<>>} ->
				ok;
			{Reg, _Offset, BinAcc} ->
				ok = gen_statem:call(Pid, {write_coils, Reg, BinAcc})
		end
	end, ReqList), ok.

