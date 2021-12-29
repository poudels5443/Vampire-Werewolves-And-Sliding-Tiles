%%%%%%%%          Name: Safal Poudel           %%%%%%%%
%%%%%%%%            Intro to AI                %%%%%%%%
%%%%%%%% Sliding Tile & Vampire Wolves Problem %%%%%%%%


%%%%%%% State representation vampire werewolf problem %%%%
validMove(Ve, We, Vw, Ww) :-
	Ve>=0, Ve=<3,
	Vw>=0, Vw=<3,
	We>=0, We=<3,
	Ww>=0, Ww=<3,
    (Ve>=We ; Ve=0),
	(Vw>=Ww ; Vw=0).

%%%%% Two werewolves going from east to west %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,e],[NewVe,NewWe,NewVw,NewWw,w]) :-
	NewVe is Ve,
	NewVw is Vw,
	NewWe is We-2,
	NewWw is Ww+2,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% Two vampires going from east to west %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,e],[NewVe,NewWe,NewVw,NewWw,w]) :-
	NewVe is Ve-2,
	NewVw is Vw+2,
	NewWe is We,
	NewWw is Ww,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% One werewolf, one vampire going from east to west %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,e],[NewVe,NewWe,NewVw,NewWw,w]) :-
	NewVe is Ve-1,
	NewVw is Vw+1,
	NewWe is We-1,
	NewWw is Ww+1,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% One vampire going from east to west %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,e],[NewVe,NewWe,NewVw,NewWw,w]) :-
	NewVe is Ve-1,
	NewVw is Vw+1,
	NewWe is We,
	NewWw is Ww,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% One werewolf going from east to west %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,e],[NewVe,NewWe,NewVw,NewWw,w]) :-
	NewVe is Ve,
	NewVw is Vw,
	NewWe is We-1,
	NewWw is Ww+1,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% Two vampires going from west to east %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,w],[NewVe,NewWe,NewVw,NewWw,e]) :-
	NewVe is Ve+2,
	NewVw is Vw-2,
	NewWe is We,
	NewWw is Ww,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% Two werewolves going from west to east %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,w],[NewVe,NewWe,NewVw,NewWw,e]) :-
	NewVe is Ve,
	NewVw is Vw,
	NewWe is We+2,
	NewWw is Ww-2,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% One werewolf, one vampire going from west to east %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,w],[NewVe,NewWe,NewVw,NewWw,e]) :-
	NewVe is Ve+1,
	NewVw is Vw-1,
	NewWe is We+1,
	NewWw is Ww-1,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% One vampire going from west to east %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,w],[NewVe,NewWe,NewVw,NewWw,e]) :-
	NewVe is Ve+1,
	NewVw is Vw-1,
	NewWe is We,
	NewWw is Ww,
	validMove(NewVe,NewWe,NewVw,NewWw).

%%%%% One werewolf going from west to east %%%%
mov(vamp-wolves, [Ve,We,Vw,Ww,w],[NewVe,NewWe,NewVw,NewWw,e]) :-
	NewVe is Ve,
	NewVw is Vw,
	NewWe is We+1,
	NewWw is Ww-1,
	validMove(NewVe,NewWe,NewVw,NewWw).


%%%%%%%%%%% State representation of sliding tile puzzle %%%%%%%%
% left moves
mov(sliding-tile, [A,0,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [0,A,C,D,E,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,0,D,E,F,G,H,I,J,K,L,M,N,O,P], [A,0,B,D,E,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,0,E,F,G,H,I,J,K,L,M,N,O,P], [A,B,0,C,E,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,0,G,H,I,J,K,L,M,N,O,P], [A,B,C,D,0,E,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,0,H,I,J,K,L,M,N,O,P], [A,B,C,D,E,0,F,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,0,I,J,K,L,M,N,O,P], [A,B,C,D,E,F,0,G,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,0,K,L,M,N,O,P], [A,B,C,D,E,F,G,H,0,I,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,0,L,M,N,O,P], [A,B,C,D,E,F,G,H,I,0,J,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,0,M,N,O,P], [A,B,C,D,E,F,G,H,I,J,0,K,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,0,O,P], [A,B,C,D,E,F,G,H,I,J,K,L,0,M,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,0,P], [A,B,C,D,E,F,G,H,I,J,K,L,M,0,N,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,0], [A,B,C,D,E,F,G,H,I,J,K,L,M,N,0,O]).

% right moves
mov(sliding-tile, [0,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [B,0,C,D,E,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,0,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [A,C,0,D,E,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,0,D,E,F,G,H,I,J,K,L,M,N,O,P], [A,B,D,0,E,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,0,F,G,H,I,J,K,L,M,N,O,P], [A,B,C,D,F,0,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,0,G,H,I,J,K,L,M,N,O,P], [A,B,C,D,E,G,0,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,0,H,I,J,K,L,M,N,O,P], [A,B,C,D,E,F,H,0,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,0,J,K,L,M,N,O,P], [A,B,C,D,E,F,G,H,J,0,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,0,K,L,M,N,O,P], [A,B,C,D,E,F,G,H,I,K,0,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,0,L,M,N,O,P], [A,B,C,D,E,F,G,H,I,J,L,0,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,0,N,O,P], [A,B,C,D,E,F,G,H,I,J,K,L,N,0,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,0,O,P], [A,B,C,D,E,F,G,H,I,J,K,L,M,O,0,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,0,P], [A,B,C,D,E,F,G,H,I,J,K,L,M,N,P,0]).

% up
mov(sliding-tile, [A,B,C,D,0,F,G,H,I,J,K,L,M,N,O,P], [0,B,C,D,A,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,0,G,H,I,J,K,L,M,N,O,P], [A,0,C,D,E,B,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,0,H,I,J,K,L,M,N,O,P], [A,B,0,D,E,F,C,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,0,I,J,K,L,M,N,O,P], [A,B,C,0,E,F,G,D,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,0,J,K,L,M,N,O,P], [A,B,C,D,0,F,G,H,E,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,0,K,L,M,N,O,P], [A,B,C,D,E,0,G,H,I,F,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,0,L,M,N,O,P], [A,B,C,D,E,F,0,H,I,J,G,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,0,M,N,O,P], [A,B,C,D,E,F,G,0,I,J,K,H,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,0,N,O,P], [A,B,C,D,E,F,G,H,0,J,K,L,I,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,0,O,P], [A,B,C,D,E,F,G,H,I,0,K,L,M,J,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,0,P], [A,B,C,D,E,F,G,H,I,J,0,L,M,N,K,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,0], [A,B,C,D,E,F,G,H,I,J,K,0,M,N,O,L]).

% down
mov(sliding-tile, [0,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [E,B,C,D,0,F,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,0,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [A,F,C,D,E,0,G,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,0,D,E,F,G,H,I,J,K,L,M,N,O,P], [A,B,G,D,E,F,0,H,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,0,E,F,G,H,I,J,K,L,M,N,O,P], [A,B,C,H,E,F,G,0,I,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,0,F,G,H,I,J,K,L,M,N,O,P], [A,B,C,D,I,F,G,H,0,J,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,0,G,H,I,J,K,L,M,N,O,P], [A,B,C,D,E,J,G,H,I,0,K,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,0,H,I,J,K,L,M,N,O,P], [A,B,C,D,E,F,K,H,I,J,0,L,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,0,I,J,K,L,M,N,O,P], [A,B,C,D,E,F,G,L,I,J,K,0,M,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,0,J,K,L,M,N,O,P], [A,B,C,D,E,F,G,H,M,J,K,L,0,N,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,0,K,L,M,N,O,P], [A,B,C,D,E,F,G,H,I,N,K,L,M,0,O,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,0,L,M,N,O,P], [A,B,C,D,E,F,G,H,I,J,O,L,M,N,0,P]).
mov(sliding-tile, [A,B,C,D,E,F,G,H,I,J,K,0,M,N,O,P], [A,B,C,D,E,F,G,H,I,J,K,P,M,N,O,0]).


%%%% heuristic definition of sliding tile i.e. number of misplaced tiles  %%%%%
heuristic(sliding-tile,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], [Aa,Bb,Cc,Dd,Ee,Ff,Gg,Hh,Ii,Jj,Kk,Ll,Mm,Nn,Oo,Pp], Hvalue, [Ha,Hb,Hc,Hd,He,Hf,Hg,Hhh,Hi,Hj,Hk,Hl,Hm,Hn,Ho,Hp]):-
    (A==Aa -> Ha is 0; Ha is 1),
    (B==Bb -> Hb is 0; Hb is 1),
    (C==Cc -> Hc is 0; Hc is 1),
    (D==Dd -> Hd is 0; Hd is 1),
    (E==Ee -> He is 0; He is 1),
    (F==Ff -> Hf is 0; Hf is 1),
    (G==Gg -> Hg is 0; Hg is 1),
    (H==Hh -> Hhh is 0; Hhh is 1),
    (I==Ii -> Hi is 0; Hi is 1),
    (J==Jj -> Hj is 0; Hj is 1),
    (K==Kk -> Hk is 0; Hk is 1),
    (L==Ll -> Hl is 0; Hl is 1),
    (M==Mm -> Hm is 0; Hm is 1),
    (N==Nn -> Hn is 0; Hn is 1),
    (O==Oo -> Ho is 0; Ho is 1),
    (P==Pp -> Hp is 0; Hp is 1),
    Hvalue is Ha+Hb+Hc+Hd+He+Hf+Hg+Hhh+Hi+Hj+Hk+Hl+Hm+Hn+Ho+Hp.

%%%%% heuristic definition of vampire werewolves

heuristic(vamp-wolves,[_,_,Vw,Ww,_],[_,_,_,_,_], H,_) :-
    H is (6-Ww-Vw).



empty_sort_queue([]).


%%%% BFS, DFS, HFS are implemented using Luger's code %%%%
%%%DFS Implementation %%%%
goDFS(Puzzle,Start, Goal) :-
	empty_stack(Empty_been_list),
	stack(Start, Empty_been_list, Been_list),
	pathDFS(Puzzle,Start, Goal, Been_list).

	% path implements a depth first search in PROLOG

	% Current state = goal, print out been list
pathDFS(_,Goal, Goal, Been_list) :-
	reverse_print_stack(Been_list).

pathDFS(Puzzle,State, Goal, Been_list) :-
	mov(Puzzle,State, Next),
	% not(unsafe(Next)),
	not(member_stack(Next, Been_list)),
	stack(Next, Been_list, New_been_list),
	pathDFS(Puzzle,Next, Goal, New_been_list), !.

reverse_print_stack(S) :-
	empty_stack(S).
reverse_print_stack(S) :-
	stack(E, Rest, S),
	reverse_print_stack(Rest),
	write(E), nl.
empty_stack([]).
member_stack(E, S) :- member(E, S).
stack(E, S, [E|S]).


%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%BFS Implementation %%%%%%%%

member_queue(E, S) :- member(E, S).
member_set(E, S) :- member(E, S).

add_to_set(X, S, S) :- member(X, S), !.
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T) :- !.
remove_from_set(E, [H|T], [H|T_new]) :-
    remove_from_set(E, T, T_new), !.

set_diff([], _, []).
set_diff([H|T], S, T_new) :-
    member_set(H, S),
    set_diff(T, S, T_new),!.
set_diff([H|T], S, [H|T_new]) :-
    set_diff(T, S, T_new), !.


equal_set(S1, S2) :-
    subset(S1, S2), subset(S2, S1).

state_record(State, Parent, [State, Parent]).

goBFS(Puzzle,Start, Goal) :-
    empty_queue(Empty_open),
    state_record(Start, nil, State),
    add_to_queue(State, Empty_open, Open),
    empty_set(Closed),
    pathBFS(Puzzle,Open, Closed, Goal).

pathBFS(_,Open,_,_) :- empty_queue(Open),
                  write('graph searched, no solution found').

pathBFS(_,Open, Closed, Goal) :-
    remove_from_queue(Next_record, Open, _),
    state_record(State, _, Next_record),
    State = Goal,
    write('Solution path is: '), nl,
    printsolutionBFS(Next_record, Closed).

pathBFS(Puzzle,Open, Closed, Goal) :-
    remove_from_queue(Next_record, Open, Rest_of_open),
    (bagof(Child, movesBFS(Puzzle,Next_record, Open, Closed, Child), Children);Children = []),
    add_list_to_queue(Children, Rest_of_open, New_open),
    add_to_set(Next_record, Closed, New_closed),
    pathBFS(_,New_open, New_closed, Goal),!.

movesBFS(Puzzle,State_record, Open, Closed, Child_record) :-
    state_record(State, _, State_record),
    mov(Puzzle,State, Next),
    % not (unsafe(Next)),
    state_record(Next, _, Test),
    not(member_queue(Test, Open)),
    not(member_set(Test, Closed)),
    state_record(Next, State, Child_record).

printsolutionBFS(State_record, _):-
    state_record(State,nil, State_record),
    write(State), nl.
printsolutionBFS(State_record, Closed) :-
    state_record(State, Parent, State_record),
    state_record(Parent, _, Parent_record),
    member(Parent_record, Closed),
    printsolutionBFS(Parent_record, Closed),
    write(State), nl.

add_list_to_queue([], Queue, Queue).
add_list_to_queue([H|T], Queue, New_queue) :-
    add_to_queue(H, Queue, Temp_queue),
    add_list_to_queue(T, Temp_queue, New_queue).

empty_queue([]).

       % add_to_queue adds a new element to the back of the queue

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

    % remove_from_queue removes the next element from the queue
    % Note that it can also be used to examine that element
    % without removing it

remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :-
    append(First, Second, Concatenation).
empty_set([]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%HFS Implementation %%%%%%%%%%%%%%%



member_sort_queue(E, S) :- member(E, S).
insert_sort_queue(State, [], [State]).
insert_sort_queue(State, [H | T], [State, H | T]) :-
    precedes(State, H).
insert_sort_queue(State, [H|T], [H | T_new]) :-
    insert_sort_queue(State, T, T_new).

remove_sort_queue(First, [First|Rest], Rest).

state_record(State, Parent, G, H, F, [State, Parent, G, H, F]).
precedes([_,_,_,_,F1], [_,_,_,_,F2]) :- F1 =< F2.


goHFS(Puzzle,Start, Goal) :-
    empty_set(Closed),
    empty_sort_queue(Empty_open),
    heuristic(Puzzle,Start, Goal, H,_),
    state_record(Start, nil, 0, H, H, First_record),
    insert_sort_queue(First_record, Empty_open, Open),
    pathHFS(Puzzle,Open,Closed, Goal).

pathHFS(_,Open,_,_) :-
    empty_sort_queue(Open),
    write("graph searched, no solution found").

    % The next record is a goal
    % Print out the list of visited states
pathHFS(_,Open, Closed, Goal) :-
    remove_sort_queue(First_record, Open, _),
    state_record(State, _, _, _, _, First_record),
    State = Goal,
    write('Solution path is: '), nl,
    printsolution(First_record, Closed).

    % The next record is not equal to the goal
    % Generate its children, add to open and continue
    % Note that bagof in AAIS prolog fails if its goal fails,
    % I needed to use the or to make it return an empty list in this case
pathHFS(Puzzle,Open, Closed, Goal) :-
    remove_sort_queue(First_record, Open, Rest_of_open),
    (bagof(Child, movesHFS(Puzzle,First_record, Open, Closed, Child, Goal), Children);Children = []),
    insert_list(Children, Rest_of_open, New_open),
    add_to_set(First_record, Closed, New_closed),
    pathHFS(Puzzle,New_open, New_closed, Goal),!.


movesHFS(Puzzle,State_record, Open, Closed,Child, Goal) :-
    state_record(State, _, G, _,_, State_record),
    mov(Puzzle,State, Next),
    % not(unsafe(Next)),
    state_record(Next, _, _, _, _, Test),
    not(member_sort_queue(Test, Open)),
    not(member_set(Test, Closed)),
    G_new is G + 1,
    heuristic(Puzzle,Next, Goal, H,_),
    F is G_new + H,
    state_record(Next, State, G_new, H, F, Child).


insert_list([], L, L).
insert_list([State | Tail], L, New_L) :-
    insert_sort_queue(State, L, L2),
    insert_list(Tail, L2, New_L).
printsolution(Next_record, _):-
    state_record(State, nil, _, _,_, Next_record),
    write(State), nl.
printsolution(Next_record, Closed) :-
    state_record(State, Parent, _, _,_, Next_record),
    state_record(Parent, _, _, _, _, Parent_record),
    member_set(Parent_record, Closed),
    printsolution(Parent_record, Closed),
    write(State), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
