-module(users).

-export([get_users/0, get_females/1, split_by_age/1, get_id_name/1]).

get_users() ->
    [{user, 1, "Bob", male, 22},
     {user, 2, "Anna", female, 14},
     {user, 3, "Casper", male, 11},
     {user, 4, "Julia", female, 18}].

%%%
% create functions which retrieve the females from the list above
% note you need to define several "get_females()" functions for different
% list configurations

get_females(Users) -> [].



% create functions which split the users in 2 groups: those
% younger than 18 and older dan 18
split_by_age(Users) -> [].

%create functions to return the id and names as tuple of the users list above.


get_id_name(Users) -> [].
