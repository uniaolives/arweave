-module(ar_avalon_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ar_avalon.hrl").

state_vector_test() ->
	Result = ar_avalon:state_vector(7, 7, 7),
	?assertMatch(#{ magnitude := _, phase_angle := _, coherence := _ }, Result),
	Coherence = maps:get(coherence, Result),
	?assertEqual(21.0 / 30.0, Coherence).

anchor_test() ->
	Result = ar_avalon:anchor(0, 7.0),
	?assertEqual(<<"NEW_BASELINE_CONSECRATED">>, maps:get(status, Result)),
	Barrier = maps:get(exclusion_function, Result),
	?assertEqual(infinity, Barrier(5.0)),
	?assertEqual(0, Barrier(8.0)).

mine_silence_test() ->
	%% This test might fail or succeed based on random values,
	%% but we just want to see if it runs.
	Result = ar_avalon:mine_silence(1),
	case Result of
		{error, no_insight_found} -> ok;
		#{ nonce := _ } -> ok
	end.
