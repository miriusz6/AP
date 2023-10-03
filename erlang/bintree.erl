-module(bintree).
-compile(export_all).
%-export([move/2,robot]).


% {type, key, val, Left, Right}



create({Key,Value})->{Key,Value,empty,empty}.

insert({K, V , empty, R}, {NewKey,NewVal}) when K > NewKey -> 
    {K, V, {NewKey,NewVal,empty,empty}, R};
insert({K, V , L, empty}, {NewKey,NewVal}) when K =< NewKey -> 
    {K, V, L, {NewKey,NewVal,empty,empty}};
insert({K, V , L, R}, X = {NewKey,NewVal}) when K > NewKey -> 
    {K, V, insert(L,X), R};
insert({K, V , L, R}, X = {NewKey,NewVal}) when K =< NewKey -> 
    {K, V, L, insert(R,X)}.

getVal({K, V , _, _},Key) when K == Key -> V ;
getVal({K, _, empty, _},Key) when K =/= Key, K > Key ->
    missingVALUE;
getVal({K, _, _, empty},Key) when K =/= Key, K =< Key ->
    missingVALUE;
getVal({K, _, L, _},Key) when K =/= Key, K > Key ->
    getVal(L,Key);
getVal({K, _, _, R},Key) when K =/= Key, K =< Key ->
    getVal(R,Key).




% A = bintree:create({5,first}).
% B = bintree:insert(A,{3,leftRight}).
% C = bintree:insert(B,{7,rightLeft}).
% D = bintree:insert(C,{1,leftLeft}).
% E = bintree:insert(D,{10,rightRight}).




% contains(_, leaf) -> false;
% contains(Key, {node, K, Left, Right}) ->
% 	if Key =:= K -> true;
% 	Key < K -> contains(Key, Left);
% 	Key > K -> contains(Key, Right)
% 	end.




% move(north, {X, Y}) -> {X, Y+1};
% move(west, {X, Y}) -> {X-1, Y};
% move(south, {X, Y}) -> {X, Y-1};
% move(east, {X, Y}) -> {X+1, Y};
% move(_, {X, Y}) -> bad_direction;
% move(_, _) -> bad_direction_or_init_pos.


% -record(robot, {name,
% 	type=industrial,
% 	hobbies,
% details=[]}).

% make_it(Name)->#robot{name=Name,type=handmade,details=["Moved by a small man inside"]}.

% %Robo = #robot{name="Tomek",type=handmade,details=["Moved by a small man inside"]}.


% %first_robot() ->
	
