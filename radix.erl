-module(radix).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-record(tree, {value :: maybe(),
               children :: children()}).
-type maybe() :: error | {ok, term()}.
-type tree() :: #tree{}.
-type children() :: orddict:orddict().

-spec empty() -> tree().
empty() ->
    #tree{value=error,
          children=[]}.

-spec find(string(), tree()) -> maybe().
find([], #tree{value=Value}) ->
    Value;
find([H | T], #tree{children=Children}) ->
    case orddict:find(H, Children) of
        {ok, Tree} ->
            find(T, Tree);
        error ->
            error
    end.

-spec store(string(), term(), tree()) -> tree().
store([], Value, Tree) ->
    Tree#tree{value={ok, Value}};
store([H | T], Value, Tree=#tree{children=Children}) ->
    OldVal = case orddict:find(H, Children) of
        {ok, OldTree} ->
            OldTree;
        error ->
            empty()
    end,
    NewSubTree = store(T, Value, OldVal),
    Tree#tree{children=orddict:store(H,
                                     NewSubTree,
                                     Children)}.

-spec to_list(tree()) -> [{string(), term()}].
to_list(Tree) ->
    lists:flatten(to_list("", Tree)).

to_list(Prefix, #tree{value={ok, Value},
                      children=Children}) ->
    [{Prefix, Value}] ++ [to_list(Prefix ++ [C], V) || {C, V} <- Children];
to_list(Prefix, #tree{value=error,
                      children=Children}) ->
    [to_list(Prefix ++ [C], V) || {C, V} <- Children].


%% Testing

test() ->
    eqc:quickcheck(prop_to_list()).

input_list() ->
    list({list(int()), int()}).

prop_to_list() ->
    ?FORALL(Xs, input_list(),
            to_list(insert_list(Xs)) =:= orddict:from_list(Xs)).

insert_list(L) ->
    lists:foldl(fun insert_fun/2, empty(), L).

insert_fun({Key, Value}, Acc) ->
    store(Key, Value, Acc).
