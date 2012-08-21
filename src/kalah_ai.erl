-module(kalah_ai).

-export([start/0, stop/0, process/2, step/1]).

-include("kalah_area.hrl").
-include_lib("eunit/include/eunit.hrl").

start() -> ok.

stop() -> ok.

step(State) ->
	{ Pos , _Est } = process(State, 6),
	kalah_area:step(State, Pos).

%% Вычислить следующий выгодный ход
-spec process(State :: kalah_state, Level :: non_neg_integer()) -> { my_cell(), integer() } | {ok, integer() }.
process(State, 0) ->
	{ok, case State#kalah_state.owner of
			playerB -> kalah_area:estimate(State);
			playerA -> -kalah_area:estimate(State)
		end
	}
;

process(State, Level) ->
			Seq = lists:seq(1,6),
			optimal(lists:map(
				fun(X) ->
					NewState = kalah_area:step(State, X),
					case {NewState, is_record(NewState, kalah_state)} of
						{{ error , Reason }, _ } -> { error , Reason };
						{ _, true } -> { _Pos, Est } = process(NewState, Level - 1),
							{ X , Est }
					end
				end,Seq))
.


optimal([]) -> { error, no_elements};

optimal([{error, _ } | List ]) -> optimal( List );

optimal([X | List]) -> optimal(List, X).



optimal([], X) -> X;

optimal([{error, _ } | List ], X) -> optimal( List, X );

optimal([X | List], Y) ->
	{ _ , Est1 } = X,
	{ _,  Est2 } = Y,
	case Est1 < Est2 of
		true -> optimal(List, X);
		false -> optimal(List, Y)
	end.

