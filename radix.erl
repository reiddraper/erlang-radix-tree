-module(radix).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-record(tree, {value=nothing :: maybe(),
               children=[] :: children()}).

-type maybe() :: nothing | {just, term()}.
-type tree() :: #tree{}.
-type children() :: orddict:orddict().

-spec empty() -> tree().
empty() ->
    #tree{value=nothing,
          children=[]}.

-spec find(string(), tree()) -> maybe().
find([], #tree{value={just, Value}}) ->
    {ok, Value};
find([], #tree{value=nothing}) ->
    error;
find(String, #tree{children=Children}) ->
    case find_prefix(String, Children) of
        {ok, {Prefix, Tree}} ->
            Tail = lists:nthtail(length(Prefix), String),
            find(Tail, Tree);
        error ->
            error
    end.

find_prefix(String, Orddict) ->
    orddict:fold(find_prefix_helper(String), error, Orddict).

find_prefix_helper(String) ->
    fun(Key, Value, Acc) ->
            case lists:prefix(Key, String) of
                true ->
                    {ok, {Key, Value}};
                false ->
                    Acc
            end
    end.

-spec store(string(), term(), tree()) -> tree().
store("", Value, Tree) ->
    Tree#tree{value={just, Value}};
store(String, Value, Tree=#tree{children=[]}) ->
    Tree#tree{children=[{String, #tree{value={just, Value},
                                       children=[]}}]};
store(String, Value, Tree=#tree{children=Children}) ->
    case orddict:find(String, Children) of
        {ok, Child} ->
            handle_exact_match(String, Value, Tree, Child);
        error ->
            handle_no_exact_match(String, Value, Tree)
    end.

-spec handle_exact_match(list(), term(), tree(), tree()) ->
    tree().
handle_exact_match(String, Value, Tree=#tree{children=Children}, Child) ->
    NewChild = Child#tree{value={just, Value}},
    NewChildren = orddict:store(String, NewChild, Children),
    Tree#tree{children=NewChildren}.

-spec handle_no_exact_match(string(), term(), tree()) -> tree().
handle_no_exact_match(String, Value, Tree=#tree{children=Children}) ->
    case find_splittable(String, Children) of
        {insert, {ChildKey, OldTree}} ->
            handle_insert(Tree, String, Value, ChildKey, OldTree);
        {split, {ChildKey, OldTree}} ->
            handle_split(Tree, String, Value, ChildKey, OldTree);
        nothing ->
            handle_nothing(Tree, String, Value)
    end.

-spec handle_insert(tree(), list(), term(), list(), tree()) -> tree().
handle_insert(Tree=#tree{children=Children}, String, Value, ChildKey, OldTree) ->
    Tail = lists:nthtail(length(ChildKey), String),
    ChildTree = store(Tail, Value, OldTree),
    NewChildren = orddict:store(ChildKey, ChildTree, Children),
    Tree#tree{children=NewChildren}.

-spec handle_split(tree(), list(), term(), list(), tree()) -> tree().
handle_split(Tree, String, Value, ChildKey, OldTree) ->
    SplitKey = common_prefix(String, ChildKey),
    InputTail = lists:nthtail(length(SplitKey), String),
    case InputTail of
        "" ->
            handle_split_is_exact(Tree, SplitKey, ChildKey, Value, OldTree);
        _Else ->
            handle_split_prefix(Tree, SplitKey, ChildKey, Value, OldTree, InputTail)
    end.

handle_split_is_exact(Tree=#tree{children=Children}, SplitKey, ChildKey, Value, OldTree) ->
    ChildKeyTail = lists:nthtail(length(SplitKey), ChildKey),
    SplitTree = #tree{value={just, Value},
                      children=[{ChildKeyTail,
                                 OldTree}]},
    DeletedKeyChildren = orddict:erase(ChildKey,
                                       Children),
    SplitKeyChildren = orddict:store(SplitKey,
                                     SplitTree,
                                     DeletedKeyChildren),
    Tree#tree{children=SplitKeyChildren}.

handle_split_prefix(Tree=#tree{children=Children}, SplitKey, ChildKey, Value, OldTree, InputTail) ->
    ChildKeyTail = lists:nthtail(length(SplitKey), ChildKey),
    ChildList = [{ChildKeyTail, OldTree},
                 {InputTail, #tree{value={just, Value}}}],
    NewChildren = orddict:from_list(ChildList),
    NewIntermediaryTree = #tree{children=NewChildren},
    DeletedKeyChildren = orddict:erase(ChildKey,
                                       Children),
    SplitKeyChildren = orddict:store(SplitKey,
                                     NewIntermediaryTree,
                                     DeletedKeyChildren),
    Tree#tree{children=SplitKeyChildren}.

-spec handle_nothing(tree(), list(), term()) -> tree().
handle_nothing(Tree=#tree{children=Children}, String, Value) ->
    ValueTree = #tree{value={just, Value}},
    NewChildren = orddict:store(String, ValueTree, Children),
    Tree#tree{children=NewChildren}.

find_splittable(String, Children) ->
    orddict:fold(find_split_helper(String), nothing, Children).

find_split_helper(String) ->
    fun(Key, Value, Acc) ->
            case lists:prefix(Key, String) of
                true ->
                    {insert, {Key, Value}};
                false ->
                    maybe_common_prefix(String, Key, Value, Acc)
            end
    end.

maybe_common_prefix(String, Key, Value, Acc) ->
    case common_prefix(String, Key) =/= [] of
        true ->
            {split, {Key, Value}};
        false ->
            Acc
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
    test(1000).

test(NumTimes) ->
    eqc:quickcheck(eqc:numtests(NumTimes, prop_to_list())),
    eqc:quickcheck(eqc:numtests(NumTimes, prop_find())).

ascii() ->
    choose(97, 107).

ascii_list() ->
    ?LET({A, B}, {list(ascii()), list(ascii())}, A ++ B).

input_list() ->
    list({ascii_list(), int()}).

%% ---------------------------------------------------------------------------

prop_to_list() ->
    ?FORALL(Xs, input_list(),
            to_list_equiv_to_orddict(Xs)).

to_list_equiv_to_orddict(Xs) ->
    to_list(from_list(Xs)) =:= orddict:from_list(Xs).

insert_fun({Key, Value}, Acc) ->
    store(Key, Value, Acc).

%% ---------------------------------------------------------------------------

list_and_element() ->
    ?LET(InputList, input_list(),
         {InputList, element_or_random(InputList)}).

element_or_random([]) ->
    list(ascii());
element_or_random(InputList) ->
    frequency([{4, elements([K || {K, _V} <- InputList])},
               {1, list(ascii())}]).

prop_find() ->
    ?FORALL({Xs, Element}, list_and_element(),
            find_equiv_to_orddict(Xs, Element)).

find_equiv_to_orddict(Xs, Element) ->
    Radix = from_list(Xs),
    Orddict = orddict:from_list(Xs),
    find(Element, Radix) =:= orddict:find(Element, Orddict).

from_list(L) ->
    lists:foldl(fun insert_fun/2, empty(), L).
