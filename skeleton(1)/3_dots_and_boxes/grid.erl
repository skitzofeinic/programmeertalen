-module(grid).
-export([show_hlines/2, show_vlines/2, print/1]).

% TODO: The other functions.

% TODO
show_hlines(Row, Grid) -> "". 
% TODO
show_vlines(Row, Grid) -> "".


% Prints this grid in a structured format
% using the show_Xlines functions.
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
