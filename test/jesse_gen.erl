%%%-----------------------------------------------------------------------------
%%% @doc Quickcheck generators for Jesse
%%%
%%% @author Dmitrii Dimandt
%%%-----------------------------------------------------------------------------
-module(jesse_gen).

%%_* Exports ===================================================================

%% JSON types
-export([ gen/1
        , any/0
        , array/0
        , boolean/0
        , integer/0
        , float/0
        , null/0
        , number/0
        , object/0
        , string/0
        , value/0
        ]).

%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").

%%_* API =======================================================================

%%_* JSON Types ----------------------------------------------------------------

gen(Type) -> ?MODULE:Type().

value() ->
  oneof([number(), string(), array(), object()]).

any() ->
  value().

%% setting vector size to more than three makes the generation very slow
%% we still get nice nested arays even in this case
array() ->
  ?SIZED(N, case N > 2 of
              true  -> vector(2, ?LAZY(value()));
              false -> vector(N, ?LAZY(value()))
            end).

boolean() ->
  bool().

null() ->
  null.

number() ->
  oneof([integer(), float()]).

integer() ->
  int().

float() ->
  real().

object() ->
  {[{string(), ?LAZY(value())}]}.

string() ->
  utf8().
