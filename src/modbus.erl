%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus-tcp devices on an ethernet network

-module(modbus).
-export([
	connect/3,
	disconnect/1,
	read_coils/3,
	read_inputs/3,
	read_ireg/3,
	read_hreg/3,
	read_coils/4,
	read_inputs/4,
	read_ireg/4,
	read_hreg/4,
	read_memory/2,
	read_memory/3,
	write_hreg/3
]).

-define(TIMEOUT, 3000).


%%% %%% -------------------------------------------------------------------
%% Basic Modbus functions
%%% %%% -------------------------------------------------------------------

%% @doc Function to connect with the modbus device.
%% @end
-spec connect(Host::string(), Port::integer(), DeviceAddr::integer()) -> {ok, pid()} | {error, term()}.
connect(Host, Port, DeviceAddr) ->
	gen_server:start(modbus_device, [Host, Port, DeviceAddr],[{timeout, ?TIMEOUT}]).

%% @doc Function to disconnect the modbus device.
%% @end
-spec disconnect(Pid::pid()) -> ok.
disconnect(Pid) ->
	gen_server:cast(Pid, stop).

%% @doc Function to request coils from the modbus device.
%% @end
-spec read_coils(Pid::pid(), Start::integer(), Offset::integer()) -> [0|1].
read_coils(Pid, Start, Offset) ->
	read_coils(Pid, Start, Offset, []).

%% @doc Function to request coils from the modbus device.
%% @end
-spec read_coils(Pid::pid(), Start::integer(), Offset::integer(), Opts::list()) -> [0|1].
read_coils(Pid, Start, Offset, Opts) ->
	gen_server:call(Pid, {read_coils, Start, Offset, Opts}).

%% @doc Function to request inputs from the modbus device.
%% @end
-spec read_inputs(Pid::pid(), Start::integer(), Offset::integer()) -> [0|1].
read_inputs(Pid, Start, Offset) ->
	read_inputs(Pid, Start, Offset, []).

%% @doc Function to request inputs from the modbus device.
%% @end
-spec read_inputs(Pid::pid(), Start::integer(), Offset::integer(), Opts::list()) -> [0|1].
read_inputs(Pid, Start, Offset, Opts) ->
	gen_server:call(Pid, {read_inputs, Start, Offset, Opts}).

%% @doc Function to request holding registers from the modbus device.
%% @end
-spec read_hreg(Pid::pid(), Start::integer(), Offset::integer()) -> [integer()].
read_hreg(Pid, Start, Offset) ->
	read_hreg(Pid, Start, Offset, []).

%% @doc Function to request holding registers from the modbus device.
%% @end
-spec read_hreg(Pid::pid(), Start::integer(), Offset::integer(), Opts::list()) ->[integer()].
read_hreg(Pid, Start, Offset, Opts) ->
	gen_server:call(Pid, {read_hreg, Start, Offset, Opts}).

%% @doc Function to request input registers from the modbus device.
%% @end
-spec read_ireg(Pid::pid(), Start::integer(), Offset::integer()) ->[integer()].
read_ireg(Pid, Start, Offset) ->
	read_ireg(Pid, Start, Offset, []).

%% @doc Function to request input registers from the modbus device.
%% @end
-spec read_ireg(Pid::pid(), Start::integer(), Offset::integer(), Opts::list()) ->[integer()].
read_ireg(Pid, Start, Offset, Opts) ->
	gen_server:call(Pid, {read_ireg, Start, Offset, Opts}).

%% @doc Function to write data on holding registers from the modbus device.
%% @end
-spec write_hreg(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_hreg(Pid, Start, Value) ->
	gen_server:call(Pid, {write_hreg, Start, Value }).


%%% %%% -------------------------------------------------------------------
%% Extended functions
%%% %%% -------------------------------------------------------------------

%% @doc Function to request a memory position from the modbus device.
%% @end
-spec read_memory(Pid::pid(), string(), Offset::integer()) -> number() | [number()].
read_memory(Pid, "%MD0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum) *32,
	gen_server:call(Pid, {read_coils, Reg, Offset*32, [{output, float32}]});

read_memory(Pid, "%MW0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum) *16,
	gen_server:call(Pid, {read_coils, Reg, Offset*16, [{output, int16}]});

read_memory(Pid, "%MB0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum) *8,
	gen_server:call(Pid, {read_coils, Reg, Offset*8, [{output, ascii}]});

read_memory(Pid, "%MX0." ++ PosNum, Offset) ->
	[Word, Bit] = string:tokens(PosNum, "."),
	Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
	gen_server:call(Pid, {read_coils, Reg, Offset, [{output, coils}]}).


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
				Data = gen_server:call(Pid, {read_coils, Reg, Offset, [{output, binary}]}),
				BinaryData = erlang:list_to_binary(Data),
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
				end, {Acc, BinaryData}, lists:reverse(MemList)),
				ResultList
		end
	end, [], ReqList).

