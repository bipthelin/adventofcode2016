%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 1: No Time for a Taxicab ---
%%%
%%%      Santa's sleigh uses a very high-precision clock to guide its
%%%      movements", "and the clock's oscillator is regulated by stars.
%%%      Unfortunately", "the stars have been stolen... by the Easter Bunny.
%%%      To save Christmas", "Santa needs you to retrieve all fifty stars by
%%%      December 25th.
%%%
%%%      Collect stars by solving puzzles. Two puzzles will be made available
%%%      on each day in the advent calendar; the second puzzle is unlocked
%%%      when you complete the first. Each puzzle grants one star. Good luck!
%%%
%%%      You're airdropped near Easter Bunny Headquarters in a city somewhere.
%%%      "Near"", "unfortunately", "is as close as you can get - the instructions
%%%      on the Easter Bunny Recruiting Document the Elves intercepted start
%%%      here", "and nobody had time to work them out further.
%%%
%%%      The Document indicates that you should start at the given
%%%      coordinates (where you just landed) and face North. Then", "follow the
%%%      provided sequence: either turn left (L) or right (R) 90 degrees", "then
%%%      walk forward the given number of blocks", "ending at a new intersection.
%%%
%%%      There's no time to follow such ridiculous instructions on foot,
%%%      though", "so you take a moment and work out the destination. Given that
%%%      you can only walk on the street grid of the city,
%%%      how far is the shortest path to the destination?
%%%
%%%      For example:
%%%
%%%      Following R2", "L3 leaves you 2 blocks East and 3 blocks North,
%%%      or 5 blocks away.
%%%      R2", "R2", "R2 leaves you 2 blocks due South of your starting position,
%%%      which is 2 blocks away.
%%%      R5", "L5", "R5", "R3 leaves you 12 blocks away.
%%%      How many blocks away is Easter Bunny HQ?
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(one).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/0]).
-export([part2/0]).

%%%_* Code =============================================================
%%%_* Macros ===========================================================
-define(input, [ "R3", "L5", "R2", "L1", "L2", "R5", "L2", "R2", "L2", "L2"
               , "L1", "R2", "L2", "R4", "R4", "R1", "L2", "L3", "R3", "L1"
               , "R2", "L2", "L4", "R4", "R5", "L3", "R3", "L3", "L3", "R4"
               , "R5", "L3", "R3", "L5", "L1", "L2", "R2", "L1", "R3", "R1"
               , "L1", "R187", "L1", "R2", "R47", "L5", "L1", "L2", "R4"
               , "R3", "L3", "R3", "R4", "R1", "R3", "L1", "L4", "L1", "R2"
               , "L1", "R4", "R5", "L1", "R77", "L5", "L4", "R3", "L2", "R4"
               , "R5", "R5", "L2", "L2", "R2", "R5", "L2", "R194", "R5", "L2"
               , "R4", "L5", "L4", "L2", "R5", "L3", "L2", "L5", "R5", "R2"
               , "L3", "R3", "R1", "L4", "R2", "L1", "R5", "L1", "R5", "L1"
               , "L1", "R3", "L1", "R5", "R2", "R5", "R5", "L4", "L5", "L5"
               , "L5", "R3", "L2", "L5", "L4", "R3", "R1", "R1", "R4", "L2"
               , "L4", "R5", "R5", "R4", "L2", "L2", "R5", "R5", "L5", "L2"
               , "R4", "R4", "L4", "R1", "L3", "R1", "L1", "L1", "L1", "L4"
               , "R5", "R4", "L4", "L4", "R5", "R3", "L2", "L2", "R3", "R1"
               , "R4", "L3", "R1", "L4", "R3", "L3", "L2", "R2", "R2", "R2"
               , "L1", "L4", "R3", "R2", "R2", "L3", "R2", "L3", "L2", "R4"
               , "L2", "R3", "L4", "R5", "R4", "R1", "R5", "R3"
               ]).

%%%_ * API -------------------------------------------------------------
part1() ->
  io:format("~p", [part1({0,0,0,0}, 0, ?input)]).

part2() ->
  io:format("~p", [part2({[{0, 0}], {0,0,0,0}}, 0, ?input)]).

%%%_* Private functions ================================================
part1({N,E,S,W}, _, []) ->
  abs(N-S)+abs(E-W);
part1(C, Dir, [H|T]) ->
  {E, Dist} =
    case H of
      "R"++D -> {mod(Dir+1, 4), list_to_integer(D)};
      "L"++D -> {mod(Dir-1, 4), list_to_integer(D)}
    end,
  part1(setelement(E+1, C, element(E+1, C) + Dist), E, T).

part2(_, _, []) ->
  "No intersection found";
part2({Coords, C}, Dir, [H|T]) ->
  {E, Dist} =
    case H of
      "R"++D -> {mod(Dir+1, 4), list_to_integer(D)};
      "L"++D -> {mod(Dir-1, 4), list_to_integer(D)}
    end,
  case find_intersection(Coords, C, E, lists:seq(1, Dist)) of
    {ok, I}   -> I;
    {no, Ret} -> part2({Ret, setelement(E+1, C, element(E+1, C) + Dist)}, E, T)
  end.

find_intersection(Coords, _, _, [])    -> {no, Coords};
find_intersection(Coords, C, E, [D|T]) ->
  {_N,_E,_S,_W} = setelement(E+1, C, element(E+1, C) + D),
  {Y, X}        = {_N-_S, _E-_W},
  case lists:member({Y, X}, Coords) of
    true  -> {ok, abs(Y+X)};
    false -> find_intersection([{Y, X} | Coords], C, E, T)
  end.

mod(Val, Mod) -> (((Val) rem Mod) + Mod) rem Mod.

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
