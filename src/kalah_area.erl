-module(kalah_area).
-export([new/0, step/2, estimate/1, is_game_over/1, opponent_cell/1]).

-include("kalah_area.hrl").
-include_lib("eunit/include/eunit.hrl").

new() -> new(6).

new(N) -> #kalah_state{
	my = { N, N, N, N, N, N, 0 },
	opponent = { N, N, N, N, N, N, 0 },
	owner = playerA
}.

-spec step(State :: kalah_state, N :: non_neg_integer()) -> { ok, halah_state } | {error, _Reason }.
step(State, N) when N > 0, N < 7 ->
	Rest = get(State, N),
	if
		Rest == 0 -> { error, cell_is_empty };
		is_integer(Rest), Rest > 0 ->
			NewState = set(State, N, 0),
			step(NewState, N+1, Rest)
	end
.


step(State, N, 1) when N < 7 ->
	MyCell = get(State,N),
	OpponentCell = get(State, opponent_cell(N)),
	case (MyCell == 0) and (OpponentCell>0) of
		true -> % Попали на пустую нашу клетку, поэтому забираем все фишки из соседней клетки оппонента + 1 наша и кладем её в наш калах. Ход переходит сопернику.
			NewState1 = set(State,opponent_cell(N), 0),
			NewState2 = increment(NewState1, 7, OpponentCell + 1),
			change(NewState2);
		false -> step(increment(State,N,1), N+1, 0)
	end
;

% Как прошли калах соперника, кладем камешки в наши поля
step(State, 15, Rest) -> step(State, 1, Rest);

% Последний ход попал в наш калах
step(State, 8, 0) ->
	State; %% Ничего не делаем, следующий ход наш.

% Последний ход попал в произвольное место
step(State, _, 0) ->
	change(State); %% Следующий ход противника

step(State, N, Rest) ->
	NewState = increment(State, N),
	step(NewState, N+1, Rest - 1).



increment(State, N) -> increment(State, N, 1).

-spec increment(State :: kalah_state, N :: non_neg_integer(), Inc :: integer()) -> kalah_state.
increment(State, N, Inc) ->
	set(State, N, get(State, N) + Inc).

-spec get(State :: kalah_state, N :: non_neg_integer()) -> integer().
get(State, N) when N>0, N<15 ->
	case N<8 of
		true -> element(N, State#kalah_state.my);
		false -> element(N - 7, State#kalah_state.opponent)
	end
.

-spec set(State :: kalah_state, N :: non_neg_integer(), Value :: integer()) -> kalah_state.
set(State, N, Value ) when N>0, N<15 ->
	case N<8 of
		true -> State#kalah_state{my = setelement(N, State#kalah_state.my, Value)};
		false -> State#kalah_state{opponent = setelement(N - 7, State#kalah_state.opponent, Value)}
	end
.

-spec change(State :: kalah_state) -> kalah_state.
change(State) ->
	#kalah_state{
		my = State#kalah_state.opponent,
		opponent = State#kalah_state.my,
		owner = case State#kalah_state.owner of
			playerA -> playerB;
			playerB -> playerA
		end
	}.

% Соседняя клетка оппонента
opponent_cell(N) when N>0, N<14 -> 14 - N.

% Оценить позицию (чем меньше, тем лучше)
-spec estimate(State :: kalah_state ) -> integer().
estimate(State) ->
	case is_game_over(State) of
		true -> -lists:sum(tuple_to_list(State#kalah_state.my)) + lists:sum(tuple_to_list(State#kalah_state.opponent));
		false ->
			-get(State, 7) + get(State, 14)
	end
.


% Проверить не закончилась ли игра
-spec isGameOver(State :: kalah_state) -> boolean().
is_game_over(State) ->
	case State#kalah_state.my of
		{ 0,0,0,0,0,0, _ } -> true;
		_ -> false
	end
.

%% ---------------------------------------------------------------------------------------------------------------------

new_test() ->
	State = new(10),
	[
		?_assertEqual(State#kalah_state.my, {10, 10, 10, 10, 10, 10, 0}),
		?_assertEqual(State#kalah_state.my, State#kalah_state.opponent),
		?_assertEqual(State#kalah_state.owner, playerA)
	].

get_test() ->
	State = new(6),
	[
		?_assertEqual(get(State,1),6),
		?_assertEqual(get(State,7),0),
		?_assertEqual(get(State,8),6),
		?_assertEqual(get(State,14),0)
	].

set_test() ->
	State = new(6),
	[
		?_assertEqual(get(set(State,1,0)),0),
		?_assertEqual(get(set(State,7,10)),10),
		?_assertEqual(get(set(State,12,120)),120),
		?_assertError(badarg,set(State,25, 10))
	].

estimate_test() ->
	State = new(6),
	GameOverState = set(new(0),7,10),
	[
		?_assertEqual(estimate(State),0),
		?_assertEqual(estimate(set(State,1,8)),0),
		?_assertEqual(estimate(set(State,7,10)),10),
		?_assertEqual(estimate(GameOverState),-10),
		?_assertEqual(estimate(set(GameOverState,14, 10)),0)
	].

step_test() ->
	State = new(6),
	[
		?_assertEqual(step(State,1),State#kalah_state{
			my = {0, 7, 7, 7, 7, 7, 1 },
			owner = playerB
		}),
		?_assertError(badarg, step(State,7))
	].

