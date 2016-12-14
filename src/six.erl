%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc --- Day 6: Signals and Noise ---
%%%
%%%      Something is jamming your communications with Santa. Fortunately,
%%%      your signal is only partially jammed, and protocol in situations
%%%      like this is to switch to a simple repetition code to get the
%%%      message through.
%%%
%%%      In this model, the same message is sent repeatedly. You've recorded
%%%      the repeating message signal (your puzzle input), but the data
%%%      seems quite corrupted - almost too badly to recover. Almost.
%%%
%%%      All you need to do is figure out which character is most frequent
%%%      for each position. For example, suppose you had recorded the
%%%      following messages:
%%%
%%%      eedadn
%%%      drvtee
%%%      eandsr
%%%      raavrd
%%%      atevrs
%%%      tsrnev
%%%      sdttsa
%%%      rasrtv
%%%      nssdts
%%%      ntnada
%%%      svetve
%%%      tesnvt
%%%      vntsnd
%%%      vrdear
%%%      dvrsen
%%%      enarar
%%%
%%%      The most common character in the first column is e; in the second,
%%%      a; in the third, s, and so on. Combining these characters returns
%%%      the error-corrected message, easter.
%%%
%%%      Given the recording in your puzzle input, what is the error-corrected
%%%      version of the message being sent?
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(six).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([part1/1]).
-export([part2/1]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
part1(FileName) ->
  io:format("~p", [solve(fun(X) -> lists:reverse(X) end, readfile(FileName))]).

part2(FileName) ->
  io:format("~p", [solve(fun(X) -> X end, readfile(FileName))]).

%%%_* Private functions ================================================
solve(MaybeFun, Lines) ->
  lists:map(
    fun(Col) ->
      element(1, hd(MaybeFun(lists:keysort(2, Col))))
    end,
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (Line, Acc) ->
        lists:zipwith(
          fun(C, Col) ->
            case lists:keyfind(C, 1, Col) of
              false  -> [{C, 1} | Col];
              {C, S} -> lists:keyreplace(C, 1, Col, {C, S+1})
            end
          end, binary_to_list(Line), Acc )
      end, [[], [], [], [], [], [], [], []], Lines) ).

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
