%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 5: How About a Nice Game of Chess? ---
%%%
%%%      You are faced with a security door designed by Easter Bunny
%%%      engineers that seem to have acquired most of their security
%%%      knowledge by watching hacking movies.
%%%
%%%      The eight-character password for the door is generated one character
%%%      at a time by finding the MD5 hash of some Door ID (your puzzle input)
%%%      and an increasing integer index (starting with 0).
%%%
%%%      A hash indicates the next character in the password if its
%%%      hexadecimal representation starts with five zeroes. If it does,
%%%      the sixth character in the hash is the next character of the password.
%%%
%%%      For example, if the Door ID is abc:
%%%
%%%      - The first index which produces a hash that starts with five zeroes
%%%        is 3231929, which we find by hashing abc3231929; the sixth character
%%%        of the hash, and thus the first character of the password, is 1.
%%%      - 5017308 produces the next interesting hash, which starts with
%%%        000008f82..., so the second character of the password is 8.
%%%      - The third time a hash starts with five zeroes is for abc5278568,
%%%        discovering the character f.
%%%
%%%      In this example, after continuing this search a total of eight times,
%%%      the password is 18f47a30.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(five).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/0]).
-export([part2/0]).

%%%_* Code =============================================================
%%%_* Macros ===========================================================
-define(input, "cxdnnyjw").

%%%_ * API -------------------------------------------------------------
part1() ->
  io:format("~p", [part1("", 0, ?input)]).

part2() ->
  io:format("~p", [part2("xxxxxxxx", 0, ?input)]).

%%%_* Private functions ================================================
part2([A,B,C,D,E,F,G,H]=Acc, _, _) when A =/= 120 andalso B =/= 120
                                andalso C =/= 120 andalso D =/= 120
                                andalso E =/= 120 andalso F =/= 120
                                andalso G =/= 120 andalso H =/= 120 ->
  Acc;
part2(Acc, E, I) ->
  case getmd5(I++integer_to_list(E)) of
    <<"00000", P:1/binary, C:1/binary, _/binary>>
      when P >= <<"0">> andalso P =< <<"7">> ->
          IP = binary_to_integer(P) + 1,
          case lists:nth(IP, Acc) of
              120 ->
                part2(
                  lists:flatten(
                    [ lists:sublist(Acc, IP-1)
                      , binary_to_list(C)
                      , lists:nthtail(IP, Acc) ]), E+1, I);
              _   -> part2(Acc, E+1, I)
          end;
    _                                        ->
          part2(Acc, E+1, I)
  end.

part1(Acc, _, _) when length(Acc) =:= 8 ->
  Acc;
part1(Acc, E, I) ->
  case getmd5(I++integer_to_list(E)) of
    <<"00000", C:1/binary, _/binary>> -> part1(Acc++binary_to_list(C), E+1, I);
    _                                 -> part1(Acc, E+1, I)
  end.

getmd5(Data) ->
  list_to_binary(
    lists:flatten(
      [io_lib:format("~2.16.0b", [X])
       || X <- binary_to_list(
                 (crypto:hash(md5, Data))) ])).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
