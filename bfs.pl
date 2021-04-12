world(9).
actor([0,0]).

:- dynamic([
    shortestPath/1,
    covid_1/1,
    covid_2/1,
    home/1,
    mask/1,
    doctor/1,
    closed/1,
    open/1
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

% GET FIRST ELEMENT FROM THE OPEN LIST
get_first_from_open(Cost, Pos, IsVaccinated, Parent) :-
    open(Open),
    Open = [H | _],
    H = [Cost, Pos, IsVaccinated, Parent].

% SORT THE OPEN LIST BY COST
sort_open :-
    open(Open),
    sort(Open, OpenS),
    retractall(open(_)),
    assert(open(OpenS)).

% GET ADJACENT POINT WITHOUT COVID
adjacent_without_covid(Pos, Adj):-
    adjacent(Pos, Adj),
    \+ is_in_covid(Adj).

% GET ALL ADJACENTS OF THE POINT
get_adjacents(Pos, Adjacents, IsVaccinated) :-
    IsVaccinated == 1,
    setof(A, adjacent(Pos, A), Adjacents).

% GET ALL ADJACENTS OF THE POINT WITHOUT COVID
get_adjacents(Pos, Adjacents, IsVaccinated) :-
    IsVaccinated == 0,
    setof(A, adjacent_without_covid(Pos, A), Adjacents).

% ADD ONE POINT TO THE OPEN LIST
%% IF ELEMENT IS IN THE CLOSED LIST THEN DO NOTHING
add_one_to_open(_, Pos, IsVaccinated, _) :-
    closed(Closed),
    member([Pos, IsVaccinated, _, _], Closed), !.

%% IF ELEMENT IS NOT IN THE OPEN LIST THEN ADD IT 
add_one_to_open(Cost, Pos, IsVaccinated, Parent) :-
    open(Open),
    \+ member([_, Pos, IsVaccinated, _], Open),
    append(Open, [[Cost, Pos, IsVaccinated, Parent]], OpenA),
    retractall(open(_)),
    assert(open(OpenA)).

%% IF ELEMENT IS IN THE OPEN LIST THEN COMPARE COST WITH OLD VALUE
%%% IF NEW COST IS LESS THEN REPLACE OLD WITH NEW
add_one_to_open(NewCost, Pos, IsVaccinated, Parent) :-
    open(Open),
    member([OldCost, Pos, IsVaccinated, OldParent], Open),
    NewCost < OldCost,
    delete(Open, [OldCost, Pos, IsVaccinated, OldParent], OpenD),
    append(OpenD, [[NewCost, Pos, IsVaccinated, Parent]], OpenA),
    retractall(open(_)),
    assert(open(OpenA)).

%%% IF NEW COST IS BIGGER THEN DO NOTHING
add_one_to_open(NewCost, Pos, IsVaccinated, _) :-
    open(Open),
    member([OldCost, Pos, IsVaccinated, _], Open),
    NewCost >= OldCost.

% ADD LIST OF POINTS TO THE OPEN LIST
%% IF LIST IS EMPTY THEN WE ARE DONE
add_to_open(ToAdd, _, _, _) :- 
    ToAdd == [].

%% ADD ONE POINT AND RECURSIVELY EXECUTE IT AGAIN WITH A REMAINING LIST
add_to_open(ToAdd, Cost, IsVaccinated, Parent) :- 
    ToAdd = [Pos | ToAdd1],
    add_one_to_open(Cost, Pos, IsVaccinated, Parent),
    add_to_open(ToAdd1, Cost, IsVaccinated, Parent).

% REMOVE FIRST ELEMENT FROM THE OPEN LIST
remove_first_from_open:-
    open(Open),
    Open = [_ | Open1],
    retractall(open(_)),
    assert(open(Open1)).

% ADD POINT TO THE CLOSED LIST 
%% NOW THIS POINT CAN BE NOT CONSIDERED IN THE FUTURE
add_to_closed(Pos, IsVaccinated, Parent, Cost) :-
    closed(Closed),
    ClosedA = [[Pos, IsVaccinated, Parent, Cost] | Closed],
    retractall(closed(_)),
    assert(closed(ClosedA)).

% BFS ALGORITHM: AT EACH STEP CONSIDER ADJACENT POINT OF THE STARTING ONE WITH THE SMALLEST COST
% UNTIL WE GOT HOME
%% IF NO ELEMENT IN THE OPEN LIST THEN WE ARE DONE
bfs:- 
    open(Open),
    length(Open, Len),
    Len == 0, !.

%% IF FIRST ELEMENT IS IN HOME THEN WE HAVE FOUND A PATH
bfs:-
    get_first_from_open(Cost, Pos, IsVaccinated, Parent),
    is_in_home(Pos),
    add_to_closed(Pos, IsVaccinated, Parent, Cost),
    retractall(open(_)),
    assert(open([])), !.

%% IF WE ARE IN THE MASK OR IN THE DOCTOR THEN WE CAN MOVE THROUGN COVID
bfs:-
    get_first_from_open(Cost, Pos, OldIsVaccinated, Parent),
    (
        (is_in_mask(Pos); is_in_doctor(Pos)) -> true
    ),
    % !FOR 2ND VARIANT OF PERCEPTION!
    % (
    %     (is_covid_near_adjacents(Pos)) -> true; true
    % ),
    IsVaccinated = 1,
    remove_first_from_open,
    add_to_closed(Pos, OldIsVaccinated, Parent, Cost),
    get_adjacents(Pos, Adjacents, IsVaccinated),
    Cost1 is Cost + 1,
    add_to_open(Adjacents, Cost1, IsVaccinated, Pos),
    sort_open,
    bfs.

%% IF WE ARE NOT IN THE MASK OR IN THE DOCTOR THEN WE CAN MOVE BYPASSING COVID
bfs:-
    get_first_from_open(Cost, Pos, IsVaccinated, Parent),
    \+ is_in_mask(Pos),
    \+ is_in_doctor(Pos),
    % !FOR 2ND VARIANT OF PERCEPTION!
    % (
    %     (is_covid_near_adjacents(Pos)) -> true; true
    % ),
    remove_first_from_open,
    add_to_closed(Pos, IsVaccinated, Parent, Cost),
    get_adjacents(Pos, Adjacents, IsVaccinated),
    Cost1 is Cost + 1,
    add_to_open(Adjacents, Cost1, IsVaccinated, Pos),
    sort_open,
    bfs.

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

% CONSTRUCT THE PATH FROM THE OBTAINED POINTS(FROM THE CLOSED LIST)
combine_path:-
    closed(Closed),
    Closed = [Point | _],
    Point = [Pos, _, _, Cost],
    home(HomePos),
    Pos == HomePos,
    trace_path(Closed, Pos, Cost).

%% IF WE GOT ALL POINTS FROM HOME TO ACTOR THEN WE ARE DONE
trace_path(_, _, -1) :-
    true.

%% IF THE POINT PRECED THE PREVIOUS ONE, THEN ADD IT TO THE PATH
trace_path(Arr, Pos, Cost):-    
    Arr = [Point | Arr1],
    Point = [Pos, _, Parent, Cost],
    shortestPath(SP),
    SP1 = [Pos | SP],
    retractall(shortestPath(_)),
    assert(shortestPath(SP1)),
    Cost1 is Cost - 1,
    trace_path(Arr1, Parent, Cost1).

%% IF THE POINT DOES NOT PRECED THE PREVIOUS ONE, THEN CHECK OTHERS
trace_path(Arr, Pos, Cost):-    
    Arr = [_ | Arr1],
    trace_path(Arr1, Pos, Cost).

main :-
    % INITIALIZATION OF THE WORLD
    make_map, 
    world(W),
    actor(A),
    assert(open([[0, A, 0, A]])),
    assert(closed([])),
    assert(shortestPath([])),
    SIZE is W - 1, 

    \+ draw_map([0, SIZE]),
    format('~nSearching...~n~n'),
    
    % START ALGORITHM AND OUTPUT THE RESULT
    get_time(T1),
    (setof(_, bfs, _), combine_path ->
        \+ draw_map([0, SIZE]),
        shortestPath(X), length(X, Count),
        Len is Count - 1,
        format('Win! Result path: ~w~nLength of the path: ~d', [X, Len]);
        format('Loss! No path'), true
    ),
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
    retractall(open(_)),
    retractall(closed(_)).
