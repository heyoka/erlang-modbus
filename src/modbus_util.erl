%%% -*- coding: utf-8 -*-
%%% %%%-------------------------------------------------------------------
%%% %%% @author J.Daniel Fernandez <jdaniel.fhermida@gmail.com>
%%% %%% @copyright (C) 2016
%%% %%% @doc ModbusTCP util
%%% %%% @end
%%% %%% -------------------------------------------------------------------
-module(modbus_util).
-export([
	binary_to_coils/1,
	binary_to_int16/1,
	binary_to_int16s/1,
	binary_to_int32/1,
	binary_to_int32s/1,
	binary_to_float32/1,
	binary_to_ascii/1,
	int16_to_binary/1
]).

%% @doc Function to convert bytes to coils.
%% @end
-spec binary_to_coils(Bin::binary()) -> [0|1].
binary_to_coils(Bin) ->
	lists:append([ lists:reverse([ Y || <<Y:1>> <= <<X>>]) || <<X:8>> <= Bin]).

%% @doc Function to convert bytes to 16bits integer.
%% @end
-spec binary_to_int16(Bin::binary()) -> [integer()].
binary_to_int16(Bin) ->
	[ X || <<X:16/integer>> <= Bin ].

%% @doc Function to convert bytes to 16bits signed integer.
%% @end
-spec binary_to_int16s(Bin::binary()) -> [integer()].
binary_to_int16s(Bin) ->
	[ X || <<X:16/signed-integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits integer.
%% @end
-spec binary_to_int32(Bin::binary()) -> [integer()].
binary_to_int32(Bin) ->
	[ X || <<X:32/integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits signed integer.
%% @end
-spec binary_to_int32s(Bin::binary()) -> [integer()].
binary_to_int32s(Bin) ->
	[ X || <<X:32/signed-integer>> <= Bin ].

%% @doc Function to convert bytes to 32bits float number.
%% @end
-spec binary_to_float32(Bin::binary()) -> [float()].
binary_to_float32(Bin) ->
	[ X || <<X:32/float>> <= Bin ].

%% @doc Function to convert bytes to ASCII.
%% @end
-spec binary_to_ascii(Bin::binary()) -> list().
binary_to_ascii(Bin) ->
	erlang:binary_to_list(Bin).

%% @private
%% @doc Function to convert a list of 16bits integer to binary.
%% @end
-spec int16_to_binary(Values::list()) -> binary().
int16_to_binary(Values) ->
    << <<X:16>> || X <- Values >>.

