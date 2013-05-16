-module(radix).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-record(tree, {value :: maybe(),
               children :: children()}).
-type maybe() :: nothing | {just, term()}.
-type tree() :: #tree{}.
-type children() :: orddict:orddict().

-spec empty() -> tree().
empty() ->
    #tree{value=nothing,
          children=[]}.

-spec find(string(), tree()) -> maybe().
find([], #tree{value=Value}) ->
    Value;
find(String, #tree{children=Children}) ->
    case find_prefix(String, Children) of
        {just, {Prefix, Tree}} ->
            Tail = lists:nthtail(length(Prefix), String),
            find(Tail, Tree);
        nothing ->
            nothing
    end.

find_prefix(String, Orddict) ->
    orddict:fold(find_prefix_helper(String), nothing, Orddict).

find_prefix_helper(String) ->
    fun(Key, Value, Acc) ->
            case lists:prefix(Key, String) of
                true ->
                    {just, {Key, Value}};
                false ->
                    Acc
            end
    end.

-spec store(string(), term(), tree()) -> tree().
store([], Value, Tree) ->
    Tree#tree{value={just, Value}};
store(String, Value, Tree=#tree{children=[]}) ->
    Tree#tree{children=[{String, #tree{value={just, Value},
                                       children=[]}}]};
store(String, Value, Tree=#tree{children=Children}) ->
    {PrefixToStore, SubTreePrefix, OldVal, NewTree} = case find_splittable(String, Children) of
        {insert, {Prefix, OldTree}} ->
            io:format("insert~n"),
            Tail = lists:nthtail(length(Prefix), String),
            {Prefix, Tail, OldTree, Tree};
        {split, {TooLong, OldTree}} ->
            io:format("splitting~n"),
            io:format("TooLong ~p~n", [TooLong]),
            io:format("OldTree ~p~n", [OldTree]),
            Prefix = common_prefix(String, TooLong),
            Tail = lists:nthtail(length(Prefix), TooLong),
            io:format("Tail ~p~n", [Tail]),

%%            {just, ValueFromTree} = OldTree#tree.value,
%%            FurtherOldTree = OldTree#tree{value=nothing},
%%            SplitTree = store(Tail, ValueFromTree, FurtherOldTree),
%%            io:format("SplitTree ~p~n", [SplitTree]),
            SplitTree = OldTree,

            StringTail = lists:nthtail(length(Prefix), String),
            {Prefix, StringTail, SplitTree, Tree#tree{children=orddict:erase(TooLong, Tree#tree.children)}};
        nothing ->
            io:format("nothing~n"),
            {String, "", empty(), Tree}
    end,
    case SubTreePrefix of
%%        "" ->
%%            NewTree#tree{children=orddict:store(PrefixToStore,
%%                                                OldVal,
%%                                                NewTree#tree.children),
%%                         value={just, Value}};
        _Else ->
            NewSubTree = store(SubTreePrefix, Value, OldVal),
            NewTree#tree{children=orddict:store(PrefixToStore,
                                                NewSubTree,
                                                NewTree#tree.children)}
    end.

find_splittable(String, Children) ->
    orddict:fold(find_split_helper(String), nothing, Children).

find_split_helper(String) ->
    fun(Key, Value, Acc) ->
            case lists:prefix(Key, String) of
                true ->
                    {insert, {Key, Value}};
                false ->
                    case common_prefix(String, Key) =/= [] of
                        true ->
                            {split, {Key, Value}};
                        false ->
                            Acc
                    end
            end
    end.

-spec common_prefix(list(A), list(A)) -> list(A).
common_prefix(A, B) ->
    common_prefix(A, B, []).


-spec common_prefix(list(A), list(A), list(A)) -> list(A).
common_prefix([], _B, Acc) ->
    Acc;
common_prefix(_A, [], Acc) ->
    Acc;
common_prefix([X | AS], [X | BS], Acc) ->
    common_prefix(AS, BS, Acc ++ [X]);
common_prefix([_A | _AS], [_B | _BS], Acc) ->
    Acc.


-spec to_list(tree()) -> [{string(), term()}].
%% @doc Return a key-sorted list. Uses a pre-order
%% traversal.
to_list(Tree) ->
    lists:flatten(to_list("", Tree)).

%% Right now we use body-recursion. Could likely rewrite to be
%% tail recursive and reverse the accumulator at the end, but this
%% is working fine for now.
to_list(Prefix, #tree{value={just, Value},
                      children=Children}) ->
    [{Prefix, Value}] ++ list_children(Prefix, Children);
to_list(Prefix, #tree{value=nothing,
                      children=Children}) ->
    list_children(Prefix, Children).

list_children(Prefix, Children) ->
    [to_list(Prefix ++ C, V) || {C, V} <- Children].

%% Testing

test() ->
    test(500).

test(NumTimes) ->
    eqc:quickcheck(eqc:numtests(NumTimes, prop_to_list())).

input_list() ->
    list({list(int()), int()}).

prop_to_list() ->
    ?FORALL(Xs, input_list(),
            equiv_to_orddict(Xs)).

equiv_to_orddict(Xs) ->
    to_list(from_list(Xs)) =:= orddict:from_list(Xs).

from_list(L) ->
    lists:foldl(fun insert_fun/2, empty(), L).

insert_fun({Key, Value}, Acc) ->
    store(Key, Value, Acc).
