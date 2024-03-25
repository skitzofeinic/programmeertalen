%% Author: Nguyen Anh Le
%% StudentID: 15000370
%% BCs Informatica

-module(grid).

-export([new/2, get_wall/3, add_wall/2, has_wall/2, show_hlines/2, show_vlines/2, 
         get_cell_walls/2, get_all_walls/2, get_open_spots/1, choose_random_wall/1, 
         print/1, thrd/1]).

%% Create a new grid with the given width and height.
new(Width, Height) ->
    {Width, Height, []}.

%% Get the coordinates of a wall based on its direction and position.
get_wall(X, Y, Direction) ->
    case Direction of
        north -> {{X, Y-1}, {X, Y}};
        east -> {{X, Y}, {X+1, Y}};
        south -> {{X, Y}, {X, Y+1}};
        west -> {{X-1, Y}, {X, Y}}
    end.

%% Check if a wall exists in the grid.
has_wall(Wall, Grid) ->
    lists:member(Wall, thrd(Grid)).

%% Add a wall to the grid.
%% True: Wall already exists, return the grid unchanged.
%% False: Append the new wall to the existing list of walls in the grid.
add_wall(Wall, Grid) ->
    case grid:has_wall(Wall, Grid) of
        true ->
            Grid;
        false ->
            {Width, Height, Walls} = Grid,
            {Width, Height, lists:append(Walls, [Wall])}
    end.

%% Get the third element of a tuple.
thrd({_, _, C}) -> C.

%% Generate horizontal lines for displaying the grid.
show_hlines(Row, Grid) ->
    {Width, _, _} = Grid,
    Hlines = lists:map(
        fun(X) ->
            case {has_wall(get_wall(X, Row, north), Grid), has_wall(get_wall(X, Row + 1, north), Grid)} of
                {true, true} -> "+--+";
                {false, true} -> "+  ";
                {true, false} -> "+--";
                {false, false} -> "+  "
            end
        end, lists:seq(0, Width-1)),
    lists:flatten(Hlines) ++ "+~n".

%% Generate vertical lines for displaying the grid.
show_vlines(Row, Grid) ->
    Width = element(1, Grid),
    VLines = lists:map(
        fun(X) ->
            case has_wall(get_wall(X, Row, west), Grid) of
                true -> "|  ";
                false -> "   "
            end
        end, lists:seq(0, Width-1)),
    case has_wall(get_wall(Width-1, Row, east), Grid) of
        true -> lists:flatten(VLines ++ "|~n");
        false -> lists:flatten(VLines ++ " ~n")
    end.

%% Print the grid in a structured format.
print(Grid) ->
    {_, H, _} = Grid,
    lists:map(fun(Row) ->
        io:fwrite(show_hlines(Row, Grid)),
        case Row < H of
            true ->
                io:fwrite(show_vlines(Row, Grid));
            false ->
                ok
        end
    end, lists:seq(0, H)),
    io:fwrite("~n"),
    ok.

%% Get the walls surrounding a cell based on its coordinates.
get_cell_walls(X, Y) ->
    [
        {{X-1, Y}, {X, Y}},
        {{X, Y-1}, {X, Y}},
        {{X, Y}, {X, Y+1}},
        {{X, Y}, {X+1, Y}}
    ].

%% Get all possible walls in the grid.
get_all_walls(W, H) ->
    AllWalls = lists:flatmap(
        fun(X) ->
            lists:flatmap(
                fun(Y) ->
                    get_cell_walls(X, Y)
                end,
                lists:seq(0, H-1))
        end,
        lists:seq(0, W-1)),
    lists:usort(lists:flatten(AllWalls)).

%% Get the open spots in the grid where walls can be placed.
get_open_spots(Grid) ->
    {W, H, Walls} = Grid,
    AllWalls = get_all_walls(W, H),
    lists:subtract(AllWalls, Walls).

%% Choose a random wall from the open spots in the grid.
choose_random_wall(Grid) ->
    OpenSpots = get_open_spots(Grid),
    case OpenSpots of
        [] ->
            [];
        _ when is_list(OpenSpots) ->
            RandomIndex = rand:uniform(length(OpenSpots)),
            lists:nth(RandomIndex, OpenSpots)
    end.
