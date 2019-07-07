%%%-----------------------------------------------------------------------------
%%% @doc
%%% modbus client reconnector.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(modbus_reconnector).

-author('miae@tgw-group.com').

-export([new/0, new/1, execute/2, reset/1]).

%% 4 seconds
-define(MIN_INTERVAL, 4).

%% 1 minute
-define(MAX_INTERVAL, 60).

-define(IS_MAX_RETRIES(Max), (is_integer(Max) orelse Max =:= infinity)).

-record(reconnector, {
          min_interval  = ?MIN_INTERVAL,
          max_interval  = ?MAX_INTERVAL,
          max_retries   = infinity,
          interval      = ?MIN_INTERVAL,
          retries       = 0,
          timer         = undefined}).

-opaque reconnector() :: #reconnector{}.

-export_type([reconnector/0]).

%%------------------------------------------------------------------------------
%% @doc Create a reconnector.
%% @end
%%------------------------------------------------------------------------------
-spec new() -> reconnector().
new() ->
    new({?MIN_INTERVAL, ?MAX_INTERVAL}).

%%------------------------------------------------------------------------------
%% @doc Create a reconnector with min_interval, max_interval seconds and max retries.
%% @end
%%------------------------------------------------------------------------------
-spec new(MinInterval) -> reconnector() when
      MinInterval  :: non_neg_integer() | {non_neg_integer(), non_neg_integer()}.
new(MinInterval) when is_integer(MinInterval), MinInterval =< ?MAX_INTERVAL ->
    new({MinInterval, ?MAX_INTERVAL});

new({MinInterval, MaxInterval}) when is_integer(MinInterval), is_integer(MaxInterval), MinInterval =< MaxInterval ->
    new({MinInterval, MaxInterval, infinity});
new({_MinInterval, _MaxInterval}) ->
    new({?MIN_INTERVAL, ?MAX_INTERVAL, infinity});
new({MinInterval, MaxInterval, MaxRetries}) when is_integer(MinInterval),
                                    is_integer(MaxInterval), ?IS_MAX_RETRIES(MaxRetries) ->
    #reconnector{min_interval = MinInterval,
                 interval     = MinInterval,
                 max_interval = MaxInterval,
                 max_retries  = MaxRetries}.

%%------------------------------------------------------------------------------
%% @doc Execute reconnector.
%% @end
%%------------------------------------------------------------------------------
-spec execute(Reconntor, TimeoutMsg) -> {stop, any()} | {ok, reconnector()} when
      Reconntor  :: reconnector(),
      TimeoutMsg :: tuple().
execute(#reconnector{retries = Retries, max_retries = MaxRetries}, _TimoutMsg) when
    MaxRetries =/= infinity andalso (Retries > MaxRetries) ->
    {stop, retries_exhausted};

execute(Reconnector=#reconnector{min_interval = MinInterval,
                                 max_interval = MaxInterval,
                                 interval     = Interval,
                                 retries      = Retries,
                                 timer        = Timer}, TimeoutMsg) ->
    % cancel timer first...
    cancel(Timer),
    % power
    Interval1 = Interval * 2,
    Interval2 =
    if
        Interval1 > MaxInterval -> MinInterval;
        true -> Interval1
    end,
    NewTimer = erlang:send_after(Interval2*1000, self(), TimeoutMsg),
    {ok, Reconnector#reconnector{interval = Interval2, retries = Retries+1, timer = NewTimer }}.

%%------------------------------------------------------------------------------
%% @doc Reset reconnector
%% @end
%%------------------------------------------------------------------------------
-spec reset(reconnector()) -> reconnector().
reset(Reconnector = #reconnector{min_interval = MinInterval, timer = Timer}) ->
    cancel(Timer),
    Reconnector#reconnector{interval = MinInterval, retries = 0, timer = undefined}.

cancel(undefined) ->ok;
cancel(Timer) when is_reference(Timer) -> erlang:cancel_timer(Timer).

