% Designed By Oscar Dahlberg for Prolog Lab 1

action(state(robot(r1, Hands), Rooms), move(r1_to_r2), NewState) :-
    member('steel_key', Hands),
    NewState=state(robot(r2,Hands), Rooms).

action(state(robot(r2, Hands), Rooms), move(r2_to_r1), NewState) :-
    member('steel_key', Hands),
    NewState=state(robot(r1,Hands), Rooms).

action(state(robot(r1, Hands), Rooms), move(r1_to_r3), NewState) :-
    member('brass_key', Hands),
    NewState=state(robot(r3,Hands), Rooms).

action(state(robot(r3, Hands), Rooms), move(r3_to_r1), NewState) :-
    member('brass_key', Hands),
    NewState=state(robot(r1,Hands), Rooms).

% Pickup
action(state(robot(Room, Hands), Rooms), pickup(Item), NewState) :-
    length(Hands, N),
    N < 2,
    select(room(Room, Items), Rooms, NRooms),
    select(Item, Items, NewItems),
    append([room(Room, NewItems)], NRooms, NewRooms),
    append([Item], Hands, NewHands),
    NewState=state(robot(Room, NewHands), NewRooms).

% Drop
action(state(robot(Room, Hands), Rooms), drop(Item), NewState) :-
    member(Item, Hands),
    select(Item, Hands, NewHands),
    select(room(Room, Items), Rooms, NRooms),
    append([Item], Items, NewItems),
    append([room(Room, NewItems)], NRooms, NewRooms),
    NewState=state(robot(Room, NewHands), NewRooms).

solve(State, _, [completed | []]) :-
    % State = state(robot(r2, Hands), _),
    State = state(_,Rooms),
    member(room('r2',Items), Rooms),
    member('package', Items).

solve(State, CurDepth, [Action | Actions]) :-
    CurDepth < 12,
    NextDepth is CurDepth + 1,
    action(State, Action, NewState),
    solve(NewState, NextDepth, Actions).

start(Actions) :- solve(state(robot(r1,[]), [room('r1', ['steel_key']), room('r2', ['brass_key']), room('r3', ['package'])]), 0, Actions).







