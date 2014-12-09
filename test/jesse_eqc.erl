%%%-------------------------------------------------------------------
%%% @doc Quickcheck tests for jesse
%%%
%%% @author Dmitrii Dimandt
%%%-------------------------------------------------------------------
-module(jesse_eqc).

%%_* Exports ===================================================================
-compile(export_all).
-export([]).

%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").

%%_* Defines ===================================================================

%% http://json-schema.org/latest/json-schema-core.html#anchor8
-define(types, [array, boolean, integer, number, null, object, string, any]).

%%_* Properties ================================================================
%%
%% @doc Property corresponds to "JSON-Schema-Test-Suite"/type.json except for
%%      the following:
%%        - types can include schemas
%%        - when types includes a schema it should fully validate the schema
%%        - types from separate schemas are merged
%%
prop_type() ->
  ?FORALL( Types
         , non_empty(list(elements(?types)))
         , ?FORALL( {Schema, SchemaTypes}
                  , generate_schema(Types)
                  , ?FORALL( Data
                           , generate_data(Types, SchemaTypes)
                           , begin
                               Validation = jesse:validate_with_schema( Schema
                                                                      , Data),
                               ModelValidation = validate_data_types( SchemaTypes
                                                                    , Data),
                               case ModelValidation of
                                 true -> equals({ok, Data}, Validation);
                                 false -> equals({ error
                                                 , [{ data_invalid
                                                    , Schema
                                                    , wrong_type
                                                    , Data
                                                    , []
                                                    }
                                                   ]
                                                 }, Validation)
                               end
                             end
                           )
                  )
         ).

%%_* Generators ================================================================
generate_schema(FromTypes) ->
  ?LET( SingleOrArray
      , oneof([single, array])
      , ?LET( Types
            , generate_types(SingleOrArray, FromTypes)
            , begin
                Str = case Types of
                        L when is_list(L) ->
                          Ts = ["\"" ++ atom_to_list(T) ++ "\"" || T <- Types],
                          "[" ++ string:join(Ts, ",") ++ "]";
                        A when is_atom(A) ->
                          "\"" ++ atom_to_list(A) ++ "\""
                      end,
                Schema = lists:flatten(["{\"type\": "
                                       , Str
                                       , "}"
                                       ]),
                { jiffy:decode(Schema)
                , case Types of [_|_] -> Types; _ -> [Types] end}
              end
            )
      ).

generate_types(single, Types) ->
  oneof(Types);
generate_types(array, Types) ->
  non_empty(list(elements(Types))).

generate_data(Types, SchemaTypes) ->
  oneof([ ?LET(T, oneof(SchemaTypes), jesse_gen:gen(T))
        , ?LET(Ts, list(elements(SchemaTypes)), [jesse_gen:gen(T) || T <- Ts])
        , ?LET(T, oneof(Types), jesse_gen:gen(T))
        , ?LET(Ts, list(elements(Types)), [jesse_gen:gen(T) || T <- Ts])
        ]).

%%_* Utility ===================================================================
validate_data_types(SchemaTypes, Data) ->
  lists:all(fun(E) -> validate_data_type(SchemaTypes, E) end, [Data]).

validate_data_type([], _) ->
  false;
validate_data_type([array | T], Data) ->
  is_list(Data) orelse validate_data_type(T, Data);
validate_data_type([boolean | T], Data) ->
  is_boolean(Data) orelse validate_data_type(T, Data);
validate_data_type([integer | T], Data) ->
  is_integer(Data) orelse validate_data_type(T, Data);
validate_data_type([number | T], Data) ->
  is_integer(Data) orelse is_float(Data) orelse validate_data_type(T, Data);
validate_data_type([null | T], Data) ->
  Data == null orelse validate_data_type(T, Data);
validate_data_type([object | T], Data) ->
  case Data of
    {[_]} -> true;
    _ -> validate_data_type(T, Data)
  end;
validate_data_type([string | T], Data) ->
  is_binary(Data) orelse validate_data_type(T, Data);
validate_data_type([any | _], _) ->
  true.

