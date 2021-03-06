%% Copyright (c) 2011 Nebularis.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(memoize).
-annotation('function').
-export([around_advice/4]).

-include_lib("annotations/include/types.hrl").

around_advice(#annotation{data=D}, M, F, Inputs) ->
    Keys = proplists:get_value(keys, D),
    Table = proplists:get_value(table, D),

    check(Table),
    case ets:lookup(Table, keys(Keys, F, Inputs)) of
        None when None == [] orelse None == false ->
            Result = annotation:call_advised(M, F, Inputs),
            true = ets:insert_new(Table, {{F, Inputs}, Result}),
            Result;
        [{_, Memoized}] ->
            Memoized
    end.

check(Table) ->
    case ets:info(Table) of
        undefined ->
            ets:new(Table, [ordered_set, public, named_table,
                            {write_concurrency,true},
                            {read_concurrency,true}, compressed]);
        _ -> ok
    end.

keys(Idx, F, Inputs) when is_integer(Idx) ->
    {F, lists:nth(Idx, Inputs)};
keys(Keys, F, Inputs) when is_list(Keys) ->
    {F, [ lists:nth(Idx, Inputs) || Idx <- Keys ]};
keys(_, F, Inputs) ->
    {F, Inputs}.
