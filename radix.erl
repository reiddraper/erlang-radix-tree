-module(radix).

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
