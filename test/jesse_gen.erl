%%%-----------------------------------------------------------------------------
%%% @doc Quickcheck generators for Jesse
%%%
%%% @author Dmitrii Dimandt
%%%-----------------------------------------------------------------------------
-module(jesse_gen).

%%_* Exports ===================================================================

%% JSON types
-export([ array/0
        , integer/0
        , float/0
        , number/0
        , string/0
        , value/0
        ]).

%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").

%%_* API =======================================================================

%%_* JSON Types ----------------------------------------------------------------
value() ->
  oneof([number(), string(), array()]).

%% setting vector size to more than three makes the generation very slow
%% we still get nice nested arays even in this case
array() ->
  ?SIZED(N, case N > 2 of
              true  -> vector(2, ?LAZY(value()));
              false -> vector(N, ?LAZY(value()))
            end).

number() ->
  oneof([integer(), float()]).

integer() ->
  int().

float() ->
  real().

string() ->
  utf8().
