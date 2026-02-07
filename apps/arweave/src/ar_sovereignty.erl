%%%
%%% @doc Sovereignty Dashboard for Arweave 4.0 monitoring.
%%%
-module(ar_sovereignty).
-behaviour(gen_server).

-export([start_link/0, update_metrics/1, generate_report/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ar_avalon.hrl").

-record(state, {
	metrics = #{
		ground_state => ?GROUND_STATE_7,
		field_coherence => 0.0,
		exclusion_rate => 0.0,
		energy_flow => 0.0,
		quantum_leaps => []
	}
}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_metrics(RealTimeData) ->
	gen_server:cast(?MODULE, {update_metrics, RealTimeData}).

generate_report() ->
	gen_server:call(?MODULE, generate_report).

init([]) ->
	{ok, #state{}}.

handle_call(generate_report, _From, State = #state{ metrics = Metrics }) ->
	GroundState = maps:get(ground_state, Metrics),
	Stability = if GroundState >= ?GROUND_STATE_7 -> <<"DIAMOND">>; true -> <<"METASTABLE">> end,
	Report = #{
		stability => Stability,
		coherence_level => maps:get(field_coherence, Metrics),
		exclusion_efficiency => maps:get(exclusion_rate, Metrics),
		total_quantum_leaps => length(maps:get(quantum_leaps, Metrics))
	},
	{reply, Report, State};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({update_metrics, RealTimeData}, State = #state{ metrics = Metrics }) ->
	%% Simulate metric calculation from RealTimeData
	NewCoherence = maps:get(coherence, RealTimeData, maps:get(field_coherence, Metrics)),
	NewExclusionRate = maps:get(exclusion_rate, RealTimeData, maps:get(exclusion_rate, Metrics)),
	NewEnergyFlow = maps:get(energy_flow, RealTimeData, maps:get(energy_flow, Metrics)),

	Leaps = maps:get(quantum_leaps, Metrics),
	NewLeaps = case maps:get(new_leap, RealTimeData, undefined) of
		undefined -> Leaps;
		Leap -> [Leap | Leaps]
	end,

	NewMetrics = Metrics#{
		field_coherence => NewCoherence,
		exclusion_rate => NewExclusionRate,
		energy_flow => NewEnergyFlow,
		quantum_leaps => NewLeaps
	},
	{noreply, State#state{ metrics = NewMetrics }};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
