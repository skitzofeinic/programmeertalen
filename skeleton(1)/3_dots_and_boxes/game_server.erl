-module(game_server).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, start_link/1]).
-export([handle_continue/2, init/1, move/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) -> gen_server:call(Pid, {move, Wall}).

init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    State = {Grid, Players},
    {ok, State, {continue, move}}.

% Informs the players that the game has finished.
finished([]) -> ok;
finished([Player | Players]) ->
    Player ! finished,
    finished(Players).

% Calculates the score of an added wall.
calculate_score(Wall, Grid) ->
    {Cell1, Cell2} = Wall,
    cell_score(Cell1, Grid) + cell_score(Cell2, Grid).

% Calculates the score of a cell.
cell_score(Cell, Grid) ->
  {X, Y} = Cell,
  FreeWalls = grid:get_cell_walls(X,Y) -- grid:thrd(Grid),
  case FreeWalls == [] of
    true -> 1;
    false -> 0
  end.

% Decides who the next player is.
decide_next_player(Players) -> lists:reverse(Players).

% Decides if a game has finished, and informs the next player to make a move.
handle_continue(move, State) ->
    {Grid, Players} = State,
    case grid:get_open_spots(Grid) == [] of
        true ->
            finished(Players),
            {stop, normal, State};
        false ->
            hd(Players) ! {move, self(), Grid},
            {noreply, State}
    end.

% Handle move call
% Adds the new wall to the state and decides the next player.
% It also calculates the score of the move.
handle_call({move, Wall}, _From, State) ->
    {Grid, Players} = State,
    NewGrid = grid:add_wall(Wall, Grid),
    NewPlayers = decide_next_player(Players),

    NewState = {NewGrid, NewPlayers},
    Score = calculate_score(Wall, NewGrid),

    {reply, {ok, Score}, NewState, {continue, move}};
% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From,
            {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.