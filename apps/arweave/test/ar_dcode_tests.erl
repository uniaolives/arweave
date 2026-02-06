-module(ar_dcode_tests).
-include_lib("eunit/include/eunit.hrl").

manifold_vector_test() ->
	Vector = ar_dcode:manifold_vector(7, 7, 7),
	?assertMatch(#{ magnitude := _, phase_angle := _, coherence := _ }, Vector),
	?assert(maps:get(coherence, Vector) > 0.6).

activate_test() ->
	{ok, Status} = ar_dcode:activate(<<"GROUND_STATE_7">>),
	?assertEqual(active, maps:get(status, Status)),
	?assertEqual(error, element(1, ar_dcode:activate(<<"INVALID">>))).

get_status_test() ->
	Status = ar_dcode:get_status(),
	?assertMatch(#{ system := _, status := _, ground_state := _ }, Status).

execute_gesture_test() ->
	Result = ar_dcode:execute_gesture(<<"first_action">>, 3),
	?assertMatch(#{ status := _, delta := _, gesture := _ }, Result),
	?assertEqual(error, element(1, ar_dcode:execute_gesture(<<"unknown">>, 3))).
