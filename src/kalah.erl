-module(kalah).

-export([start_link/0, stop/1]).

-behavior(gen_server).

-export([start_link/0,stop/1]).
-export([]).

-include("kalah_area.hrl").

-record(server, {
	state :: kalah_state(),
	game_state :: game_is_over | continue,
}).

-spec start_link() -> { ok, pid() }.
start_link() ->
	gen_server:start_link(?MODULE, [],[]).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
	gen_server:cast(Pid, stop).

step(Pid, Pos) ->
	gen_server:call(Pid, { step, Pos }).

state(Pid) ->
	gen_server:call(Pid, { state }).

init( _ ) -> #server{
	state = kalah_area:new(6),
	game_state = continue
	}
.

hanlde_call( _ , State) when State#server.game_state == game_is_over ->
	{reply, game_is_over, State }.

handle_call({step , Pos }, State) ->
	NewState = kalah_area:step(State#server.state,Pos),
	case NewState of
		{ error, Reason } -> { reply, { error, Reason }, State };
		_ -> {reply,  NewState, NewState}
	end
.

handle_call

handle_cast(stop, State) ->
	{ stop, normal, State }.

terminate(normal, State) -> ok.


