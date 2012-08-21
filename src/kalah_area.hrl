
-record(
	kalah_state, {
	my :: { integer(), integer(), integer(), integer(), integer(), integer(), integer() }, % Фишки у playerA
	opponent :: { integer(), integer(), integer(), integer(), integer(), integer(), integer() }, % Фишки у playerB
	owner :: playerA | playerB % Хозяин позиции (кто имеет текущий ход?)
}).
-type(my_cell() :: 1..7).
-type(opponent_cell() :: 8..13).
