world(9).
actor([0,0]).

:- dynamic([
    shortestPath/1,
    maxLength/1,
    covid_1/1,
    covid_2/1,
    home/1,
    mask/1,
    doctor/1
]).

% RANDOMLY SET COVID ON THE MAP
set_covid :-
    world(W),
    random(0, W, X1),
    random(0, W, Y1),
    random(0, W, X2),
    random(0, W, Y2),
    (
        (
            (X1 == X2), (Y1==Y2); actor(A), adjacent(A, [X1,Y1]); actor(A), adjacent(A, [X2, Y2]); actor([X1, Y1]); actor([X2,Y2])
        ) -> set_covid, true;
             assert(covid_1([X1, Y1])), assert(covid_2([X2, Y2]))
    ).

% RANDOMLY SET DOCTOR ON THE MAP
set_doctor :-
    world(W),
    random(0, W, X1),
    random(0, W, Y1),
    ((is_in_covid([X1, Y1]); actor([X1,Y1])) -> set_doctor, true;
    assert(doctor([X1, Y1]))).

% RANDOMLY SET MASK ON THE MAP
set_mask :-
    world(W),
    random(0, W, X1),
    random(0, W, Y1),
    ((is_in_covid([X1, Y1]); is_in_doctor([X1, Y1]); actor([X1,Y1]))-> set_mask, true;
    assert(mask([X1, Y1]))).

% RANDOMLY SET HOME ON THE MAP
set_home :-
    world(W),
    random(0, W, X1),
    random(0, W, Y1),
    ((is_in_covid([X1, Y1]); is_in_doctor([X1, Y1]); is_in_mask([X1, Y1]); actor([X1,Y1]))-> set_home, true;
    assert(home([X1, Y1]))).

% CREATE MAP RANDOMLY
make_map :-
    set_covid,
    set_doctor,
    set_mask,
    set_home.

% CHECK WHETHER GIVE POSITION IS VALID
is_valid_pos([X, Y]):- X >= 0, Y >= 0, world(W), X < W, Y < W.

% FIND ADJACENT POINT OF THE GIVEN ONE
adjacent([X, Y], [X, Y1]):- Y1 is Y - 1, is_valid_pos([X, Y1]).
adjacent([X, Y], [X, Y1]):- Y1 is Y + 1, is_valid_pos([X, Y1]).
adjacent([X, Y], [X1, Y]):- X1 is X - 1, is_valid_pos([X1, Y]).
adjacent([X, Y], [X1, Y]):- X1 is X + 1, is_valid_pos([X1, Y]).
adjacent([X, Y], [X1, Y1]):- X1 is X - 1, Y1 is Y - 1, is_valid_pos([X1, Y1]).
adjacent([X, Y], [X1, Y1]):- X1 is X - 1, Y1 is Y + 1, is_valid_pos([X1, Y1]).
adjacent([X, Y], [X1, Y1]):- X1 is X + 1, Y1 is Y - 1, is_valid_pos([X1, Y1]).
adjacent([X, Y], [X1, Y1]):- X1 is X + 1, Y1 is Y + 1, is_valid_pos([X1, Y1]).

% CHECK WHETHER POINT CONTAINS HOME/DOC/MASK/COVID
is_in_home(Point) :- home(Point).
is_in_doctor(Point) :- doctor(Point).
is_in_mask(Point) :- mask(Point).
is_in_covid([X,Y]) :- covid_1([X,Y]).
is_in_covid([X,Y]) :- covid_2([X,Y]).
is_in_covid(Point) :- covid_1(C), adjacent(C, Point).
is_in_covid(Point) :- covid_2(C), adjacent(C, Point).

% !FOR 2ND VARIANT OF PERCEPTION!
% CHECK IS COVID NEAR ADJACENTS 
% is_covid_near_adjacents(Point) :-
%     adjacent(Point, Adj),
%     adjacent(Adj, AdjOfAdj),
%     is_in_covid(AdjOfAdj).

% SET PRIORITY TO THE POINT
priority(Original, Point, Priority):-
    get_length_to_home(Original, OriginalD),
    get_length_to_home(Point, PointD),
    Priority is OriginalD - PointD.

% GET ADJACENT POINT WITHOUT COVID AND WITH PRIORITY 
prior_adjacent(Original, [A, Priority], IsVaccinated):- 
    IsVaccinated == 0,
    adjacent(Original, A),
    \+ is_in_covid(A),
    priority(Original, A, Priority).

% GET ANY ADJACENT POINT WITH PRIORITY
prior_adjacent(Original, [A, Priority], IsVaccinated):- 
    IsVaccinated == 1,
    adjacent(Original, A),
    priority(Original, A, Priority).

% GET ADJACENT POINTS IN A PRIORITIZED ORDER
prior_adjacents(Original, Adjacents, IsVaccinated):-
    bagof(A, prior_adjacent(Original, A, IsVaccinated), As),
    sort(2, >=, As, Adjacents).

% PARSE ADJACENT LIST AND GET ALL POINT FROM IT
get_point_from_adj_list(List, Point):- nth1(1, List, Point).
get_point_from_adj_list(List, Point):- nth1(2, List, Point).
get_point_from_adj_list(List, Point):- nth1(3, List, Point).
get_point_from_adj_list(List, Point):- nth1(4, List, Point).
get_point_from_adj_list(List, Point):- nth1(5, List, Point).
get_point_from_adj_list(List, Point):- nth1(6, List, Point).
get_point_from_adj_list(List, Point):- nth1(7, List, Point).
get_point_from_adj_list(List, Point):- nth1(8, List, Point).

% SHORTEST DISTANCE TO THE HOME
% (DIRECTLY, IGNORING COVID)
get_length_to_home([X,Y], Len) :-
    home([X1, Y1]),
    Len is max(abs(X1 - X), abs(Y1 - Y)).

% CHECK AND SET THE NEW FOUND PATH TO THE HOME AS THE SHORTEST ONE
%% IF THERE IS NO PATH TO HOME YET
%% THEN JUST SET NEW AS SHORTEST
set_shortest_path(Path):-
    \+ shortestPath(_),
    length(Path, L),
    retractall(maxLength(_)),
    assert(maxLength(L)),
    assert(shortestPath(Path)).

%% IF THERE IS ALREADY PATH TO HOME
%% THEN COMPARE LENGTH OF THE OLD PATH WITH THE NEW ONE
set_shortest_path(Path):-
    shortestPath(X),
    length(X, L1),
    length(Path, L2),
    L2 < L1,
    retractall(maxLength(_)),
    assert(maxLength(L2)),
    retractall(shortestPath(_)),
    assert(shortestPath(Path)).

% CHECK WHETHER GIVEN PATH IS ALREADY LONGER THAN THE SHORTEST PATH
compare_len_with_max(Path, _) :-
    length(Path, Len),
    Len == 0.

compare_len_with_max(Path, Point) :-
    length(Path, Len),
    maxLength(Max),
    get_length_to_home(Point, LenToHome),
    LenToHome + Len - 1 =< Max.

% RECURSIVE SEARCH OF THE PATH TO THE HOME
%% IF WE ARE IN THE HOME CELL 
%% THEN COMPARE PATH LENGTH WITH THE LENGTH OF EARLIER OBTAINED ONE
backtracking(Point, OldPath, _) :- 
    is_in_home(Point),
    append(OldPath, [Point], NewPath),
    compare_len_with_max(OldPath, Point),
    set_shortest_path(NewPath).

%% IF WE ARE IN THE MASK OR IN THE DOCTOR
%% THEN WE CAN GO THROUGH ANY POINT IN THE FUTURE (INCLUDING COVID)
backtracking(Point, OldPath, _) :-
    (
        (is_in_mask(Point); is_in_doctor(Point)) -> true
    ),
    % !FOR 2ND VARIANT OF PERCEPTION!
    % (
    %     (is_covid_near_adjacents(Point)) -> true; true
    % ),
    \+ member(Point, OldPath),
    compare_len_with_max(OldPath, Point),
    append(OldPath, [Point], NewPath),
    prior_adjacents(Point, Adjacents, 1),
    get_point_from_adj_list(Adjacents, [Point1, _]),
    backtracking(Point1, NewPath, 1).

%% IF WE ARE PROTECTED FROM THE COVID
%% THEN WE CAN GO THROUGH ANY POINT (INCLUDING COVID)
backtracking(Point, OldPath, 1) :-
    \+ member(Point, OldPath),
    compare_len_with_max(OldPath, Point),
    % !FOR 2ND VARIANT OF PERCEPTION!
    % (
    %     (is_covid_near_adjacents(Point)) -> true; true
    % ),
    append(OldPath, [Point], NewPath),
    prior_adjacents(Point, Adjacents, 1),
    get_point_from_adj_list(Adjacents, [Point1, _]),
    backtracking(Point1, NewPath, 1).

%% IF WE ARE NOT PROTECTED FROM THE COVID
%% THEN WE CAN GO THROUGH ANY POINT EXCEPT COVID ONE
backtracking(Point, OldPath, 0) :-
    \+ member(Point, OldPath),
    compare_len_with_max(OldPath, Point),
    % !FOR 2ND VARIANT OF PERCEPTION!
    % (
    %     (is_covid_near_adjacents(Point)) -> true; true
    % ),
    append(OldPath, [Point], NewPath),
    prior_adjacents(Point, Adjacents, 0),
    get_point_from_adj_list(Adjacents, [Point1, _]),
    backtracking(Point1, NewPath, 0).

% DRAWING MAP
draw_cell(Point) :-
    shortestPath(X),
    member(Point, X),
    \+ home(Point),
    \+ actor(Point),
    write('+ '), !.

draw_cell(Point) :-
    actor(Point),
    write('A '), !.

draw_cell(Point) :-
    is_in_covid(Point),
    write('C '), !.

draw_cell(Point) :-
    is_in_doctor(Point),
    write('D '), !.

draw_cell(Point) :-
    is_in_mask(Point),
    write('M '), !.

draw_cell(Point) :-
    is_in_home(Point),
    write('H '), !.

draw_cell(_) :-
    write('. '), !.

draw_row([X, Y]):-
    world(W),
    X < W,
    draw_cell([X, Y]),
    X1 is X+1,
    draw_row([X1, Y]).

draw_map([X,Y]) :-
    Y >= 0,
    \+ draw_row([X, Y]),
    nl,
    Y1 is Y-1,
    draw_map([X, Y1]).

main :-
    % INITIALIZATION OF THE WORLD
    make_map, 
    world(W),
    actor(A),
    Max is 2 * W, % max size of the path to the home
    SIZE is W - 1,
    assert(maxLength(Max)), 
    \+ draw_map([0, SIZE]),
    format('~nSearching...~n~n'),
    
    % START ALGORITHM AND OUTPUT THE RESULT
    get_time(T1),
    (setof(_, backtracking(A, [], 0), _) -> 
        \+ draw_map([0, SIZE]),  
        shortestPath(X), length(X, Count),
        Len is Count - 1,
        format('Win! Result path: ~w~nLength of the path: ~d', [X, Len]);
        write('Loss! No path'), true),
    get_time(T2),
    
    T is T2 - T1,
    format('~nTime spent: ~w~n~n', [T]),
    % CLEARING DYNAMIC FACTS
    retractall(covid_1(_)),
    retractall(covid_2(_)),
    retractall(home(_)),
    retractall(doctor(_)),
    retractall(mask(_)),
    retractall(shortestPath(_)),
    retractall(maxLength(_)).