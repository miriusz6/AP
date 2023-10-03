-module(trash).
-compile(export_all).
%-export([move/2,robot]).




move(north, {X, Y}) -> {X, Y+1};
move(west, {X, Y}) -> {X-1, Y};
move(south, {X, Y}) -> {X, Y-1};
move(east, {X, Y}) -> {X+1, Y};
move(_, {X, Y}) -> bad_direction;
move(_, _) -> bad_direction_or_init_pos.


-record(robot, {name,
	type=industrial,
	hobbies,
details=[]}).

make_it(Name)->#robot{name=Name,type=handmade,details=["Moved by a small man inside"]}.

%Robo = #robot{name="Tomek",type=handmade,details=["Moved by a small man inside"]}.


%first_robot() ->
	
