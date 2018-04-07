% Define module
:- module(softconstraints2,[]).

total(999999).

getGridMatches([VM|ValidMatches],ValidMatches,Grid,Total,GridMatches):-
    % total(Total),
    gridTotal(VM,Grid,NewTotal),
    NewTotal =< Total ->
    getGridMatches(ValidMatches,ValidMatches,Grid,NewTotal,GridMatches);
    select(ValidMatches,VM,GridMatches),
    getGridMatches(ValidMatches,ValidMatches,Grid,Total,GridMatches).