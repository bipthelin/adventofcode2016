%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 4: Security Through Obscurity ---
%%%
%%%      Finally, you come across an information kiosk with a list of rooms.
%%%      Of course, the list is encrypted and full of decoy data, but the
%%%      instructions to decode the list are barely hidden nearby. Better
%%%      remove the decoy data first.
%%%
%%%      Each room consists of an encrypted name (lowercase letters
%%%      separated by dashes) followed by a dash, a sector ID, and a checksum
%%%      in square brackets.
%%%
%%%      A room is real (not a decoy) if the checksum is the five most
%%%      common letters in the encrypted name, in order, with ties broken
%%%      by alphabetization. For example:
%%%
%%%      - aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common
%%%        letters are a (5), b (3), and then a tie between x, y, and z, which
%%%        are listed alphabetically.
%%%      - a-b-c-d-e-f-g-h-987[abcde] is a real room because although the
%%%        letters are all tied (1 of each), the first five are
%%%        listed alphabetically.
%%%      - not-a-real-room-404[oarel] is a real room.
%%%      - totally-real-room-200[decoy] is not.
%%%
%%%      Of the real rooms from the list above, the sum of their
%%%      sector IDs is 1514.
%%%
%%%      What is the sum of the sector IDs of the real rooms?
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(four).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/1]).
-export([part2/1]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
part1(FileName) ->
  io:format("~p", [solve_part1(readfile(FileName))]).

part2(FileName) ->
  io:format("~p", [solve_part2(readfile(FileName))]).

%%%_* Private functions ================================================
solve_part2(Lines) ->
  lists:filter(
    fun(<<>>) -> false;
       (Line) ->
      case shift_line(Line) of
        "northpole object storage" -> true;
        _                          -> false
      end
    end, Lines).

shift_line(Line) ->
  Sects          = binary:split(Line, [<<"-">>], [global]),
  [SId, _, <<>>] = binary:split(lists:last(Sects), [<<"[">>,<<"]">>], [global]),
  ShiftLength    = binary_to_integer(SId) rem 26,
  lists:flatten(
    io_lib:format("~s",
      [lists:map(
        fun(C) ->
          NewC = C + ShiftLength,
          case NewC > 122 of
            true  -> NewC - 26;
            false -> NewC
          end
        end,
        binary_to_list(
          iolist_to_binary(
            intersperse(
              32-ShiftLength, lists:sublist(Sects,length(Sects)-1)))))])).

intersperse(X, Ys) ->
  lists:reverse(tl(lists:foldl(fun(Y, Acc) -> [X, Y|Acc] end, [], Ys))).

solve_part1(Lines) ->
  lists:foldl(
    fun(Line, Sum) ->
      Sum + get_checksum(Line)
    end, 0, Lines).

get_checksum(<<>>) -> 0;
get_checksum(Line) ->
  Sects           = binary:split(Line, [<<"-">>], [global]),
  [SId, CSum, []] = lists:map(fun(O) -> binary_to_list(O) end,
                      binary:split(
                        lists:last(Sects), [<<"[">>, <<"]">>], [global])),
  Letters         = lists:sort(
                      binary_to_list(
                        iolist_to_binary(
                          lists:sublist(Sects, length(Sects)-1) ))),
  LetterCountSort =
    [X || {X, _} <-
      lists:reverse(
        lists:keysort(2,
          lists:foldl(
             fun(C, Acc) ->
               case lists:keyfind(C, 1, Acc) of
                 false  -> [{C, 1} | Acc];
                 {C, S} -> lists:keyreplace(C, 1, Acc, {C, S+1})
               end
             end, [], Letters)))],
  case lists:prefix(CSum, LetterCountSort) of
    true  -> list_to_integer(SId);
    false -> 0
  end.

readfile(FileName) ->
  {ok, Data} = file:read_file(FileName),
  binary:split(Data, [<<"\n">>], [global]).

%%%_* Editor ===========================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% vim: sw=2 ts=2 et
%%%
%%% End:
