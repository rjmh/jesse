%%%-----------------------------------------------------------------------------
%%% @doc Quickcheck tests for Jesse JSON paths
%%%
%%% @author Dmitrii Dimandt
%%% Created : 08. Dec 2014 19:10
%%%-----------------------------------------------------------------------------
-module(jesse_path_eqc).


%%_* API =======================================================================
-compile(export_all).
-export([]).

%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").

prop_path_aggregate_path() ->
  ?FORALL( ListOfItems
         , list(jesse_gen:value())
         , equals(ListOfItems, jesse_json_path:path(foo, [{foo, ListOfItems}]))
         ).

prop_path_aggregate_path_value() ->
  ?FORALL( {AggFunction, ListOfItems, Selector}
         , path_aggregate_selector(foo)
         , ?IMPLIES( aggregate_should_pass(AggFunction, ListOfItems)
                   , begin
                       Value = get_value_by_function(AggFunction, ListOfItems),
                       Filter = get_filter_by_function(dd),
                       equals( Value
                             , Filter(jesse_json_path:path( Selector
                                                          , [{foo, ListOfItems}]))
                             )
                     end
                   )
         ).

prop_path_aggregate_value() ->
  ?FORALL( {AggFunction, ListOfItems, Selector}
         , path_aggregate_function_selector()
         , ?IMPLIES( aggregate_should_pass(AggFunction, ListOfItems)
                   , begin
                       Value = get_value_by_function(AggFunction, ListOfItems),
                       Filter = get_filter_by_function(dd),
                       equals( Value
                             , Filter(jesse_json_path:value( Selector
                                                           , ListOfItems
                                                           , []))
                             )
                     end
                   )
         ).

prop_proplist_and_path() ->
  ?FORALL( Keys
         , [a,b,c,d]
         , ?FORALL( {KV, Path0}
                  , {generate_kv(Keys), generate_paths(Keys)}
                  , begin
                      Path = create_path(Path0),
                      equals( get_value_by_path(Path0, KV)
                            , jesse_json_path:path(Path, KV))
                    end
                  )
         ).

%%_* Generators ================================================================
-define(aggregate_functions_all, ?aggregate_functions_value ++
                                 [ '@unionOfObjects'
                                 , '@unionOfArrays'
                                 , '@distinctUnionOfObjects'
                                 , '@distinctUnionOfArrays']).
-define(aggregate_functions_value, [ '@count'
                                   , '@sum'
                                   , '@avg'
                                   , '@min'
                                   , '@max']).
-define(selector_types, [binary, atom, string, list]).

path_aggregate_value() ->
  ?LET( AggFunction
      , oneof(?aggregate_functions_all)
      , ?LET( Values
            , values_for_aggregate_function(AggFunction)
            , {AggFunction, Values}
            )
      ).

path_aggregate_selector(StartingSelector) ->
  ?LET( {AggFunction, Values}
      , path_aggregate_value()
      , ?LET( SelectorType
            , oneof(?selector_types)
            , { AggFunction
              , Values
              , selector(SelectorType, [StartingSelector, AggFunction])
              }
            )
      ).

path_aggregate_function_selector() ->
  ?LET( {AggFunction, Values}
      , path_aggregate_value()
      , ?LET( SelectorType
            , ?SUCHTHAT(T, oneof(?selector_types), T /= list)
            , { AggFunction
              , Values
              , selector(SelectorType, [AggFunction])
              }
            )
      ).

values_for_aggregate_function(AggFunction) ->
  case lists:member(AggFunction, ?aggregate_functions_value) of
    true -> non_empty(list(jesse_gen:number()));
    false -> list(jesse_gen:value())
  end.

generate_kv([]) ->
  [];
generate_kv([K|Keys]) ->
  ?LET( {Nest, Value}
      , {bool(), oneof([int()])}
      , begin
          case Nest of
            true -> [{K, ?LAZY(generate_kv(Keys))}];
            false -> ?LET( V
                         , ?LAZY(generate_kv(Keys))
                         , begin
                             Res = [case KV of
                                      [{K1, V1}] -> {K1, V1};
                                      _ -> KV
                                    end || KV <- V],
                              [{K, Value} | Res]
                           end)

          end
        end
      ).

generate_paths(Keys) ->
  oneof([ generate_existing_path(Keys)
        , generate_possibly_existing_path(Keys)
        , generate_random_path(Keys)
        , generate_non_existing_path()
        ]).

generate_existing_path(Keys) ->
  ?LET( N
      , oneof(lists:seq(0, length(Keys)))
      , begin
          lists:sublist(Keys, abs(N))
        end
      ).

generate_possibly_existing_path(Keys0) ->
  ?LET( LeaveKey
      , vector(length(Keys0), bool())
      , begin
          SkipKeys = lists:zip(Keys0, LeaveKey),
          [K || {K, Leave} <- SkipKeys, Leave == true]
        end
      ).

generate_random_path([]) ->
  [];
generate_random_path(Keys0) ->
  list(elements(Keys0)).

generate_non_existing_path() ->
  list(binary()).

%%_* Utility functions =========================================================

selector(binary, Selectors) ->
  list_to_binary(string:join([atom_to_list(S) || S <- Selectors], "."));
selector(atom, Selectors) ->
  list_to_atom(string:join([atom_to_list(S) || S <- Selectors], "."));
selector(string, Selectors) ->
  string:join([atom_to_list(S) || S <- Selectors], ".");
selector(list, Selectors) ->
  Selectors.

create_path(L) ->
  list_to_binary(string:join([to_string(E) || E <- L], ".")).

get_value_by_function('@count', List) ->
  length(List);
get_value_by_function('@sum', List) ->
  lists:sum(List);
get_value_by_function('@avg', []) ->
  []; %% it's weird, but that's what it is
get_value_by_function('@avg', List) ->
  lists:sum(List) / length(List);
get_value_by_function('@min', List) ->
  lists:min(List);
get_value_by_function('@max', List) ->
  lists:max(List);
get_value_by_function('@unionOfObjects', List) ->
  List;
get_value_by_function('@unionOfArrays', []) ->
  [];
get_value_by_function('@unionOfArrays', [E|_] = List) ->
  case is_list(E) of
    false -> false;
    true -> lists:foldl(fun(Element, Acc) ->
                          lists:append(Acc, Element)
                        end, [], List)
  end;
get_value_by_function('@distinctUnionOfObjects', List) ->
  lists:usort(List);
get_value_by_function('@distinctUnionOfArrays', List) ->
  lists:usort(get_value_by_function('@unionOfArrays', List)).

get_filter_by_function('@distinctUnionOfObjects') ->
  fun lists:sort/1;
get_filter_by_function('@distinctUnionOfArrays') ->
  fun lists:sort/1;
get_filter_by_function(_) ->
  fun(X) -> X end.

aggregate_should_pass('@distinctUnionOfArrays', L) ->
  L /= [] andalso lists:all(fun(E) -> is_list(E) end, L);
aggregate_should_pass('@unionOfArrays', L) ->
  lists:all(fun(E) -> is_list(E) end, L);
aggregate_should_pass(_, _) -> true.

to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(A) when is_atom(A)   -> atom_to_list(A);
to_string(L) when is_list(L)   -> L.

to_atom(B) when is_binary(B) -> binary_to_atom(B, latin1);
to_atom(A) when is_atom(A)   -> A;
to_atom(L) when is_list(L)   -> list_to_atom(L).



get_value_by_path(Keys, {L}) when is_list(L) ->
  get_value_by_path(Keys, L);
get_value_by_path(_, []) ->
  [];
get_value_by_path([], _) ->
  [];
get_value_by_path([K|Keys], KV) when is_list(KV) ->
  case proplists:get_value(to_atom(K), KV) of
    undefined -> [];
    V when Keys == [] -> V;
    Value when is_list(Value) -> get_value_by_path(Keys, Value);
    _ -> []
  end;
get_value_by_path(_, _) ->
  [].