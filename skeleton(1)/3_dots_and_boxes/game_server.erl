%% Author: Nguyen Anh Le
%% StudentID: 15000370
%% BCs Informatica

-module(game_server).

-behaviour(gen_server).

-export([
    handle_call/3,
    handle_cast/2,
    start_link/1,
    handle_continue/2,
    init/1,
    move/2
]).

%% Starts the game server.
start_link({Width, Height, Players}) ->
    gen_server:start_link(game_server, {Width, Height, Players}, []).

%% Sends a move request to the game server.
move(Pid, Wall) -> gen_server:call(Pid, {move, Wall}).

%% Initializes the game server state.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    State = {Grid, Players},
    {ok, State, {continue, move}}.

%% Handles finishing the game and informs players.
finished([]) -> ok;
finished([Player | Players]) ->
    Player ! finished,
    finished(Players).

%% Calculates the score of an added wall.
score(Wall, Grid) ->
    {C1, C2} = Wall,
    score_cell(C1, Grid) + score_cell(C2, Grid).

%% Calculates the score of a cell.
score_cell(Cell, Grid) ->
    {X, Y} = Cell,
    FreeWalls = grid:get_cell_walls(X,Y) -- grid:thrd(Grid),
    case FreeWalls == [] of
        true -> 1;
        false -> 0
    end.

%% Handles the continuation of a move action.
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

%% Handles a move request from a player.
handle_call({move, Wall}, _From, State) ->
    {Grid, Players} = State,
    NewGrid = grid:add_wall(Wall, Grid),
    NewPlayers = lists:reverse(Players),

    NewState = {NewGrid, NewPlayers},
    Score = score(Wall, NewGrid),

    {reply, {ok, Score}, NewState, {continue, move}};
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From,
            {{Width, Height, _}, Players}) ->
    {reply, ok, {{Width, Height, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
