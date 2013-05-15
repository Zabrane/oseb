% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @author CA Meijer
%% @copyright 2013 CA Meijer
%% @doc Functions for parsing JSON.
%%      

-module(openstack_json).

%% External exports
-export([get_first_value/2,
		 get_values/2,
		 select_first_value/2,
		 select_values/2]).

%% API
%% @doc Gets the first value from a JSON document associated
%%      with a specific key.
%%
%% @spec get_first_value(jsondoc(), string()) -> term()    
%% @end
get_first_value(Json, Key) ->
	case get_values(Json, Key) of
		[] ->
			undefined;
		[Value|_Rest] ->
			Value
	end.

%% @doc Gets all values from a JSON document associated
%%      with a specific key.
%%
%% @spec get_values(jsondoc(), string()) -> [term()]    
%% @end
get_values({ok, Result}=_Json, Key) ->
	get_values(Result, Key);
get_values(Json, Key) when is_list(Key) ->
	Tokens = string:tokens(Key, "."),
	KeysList = [list_to_binary(Token) || Token <- Tokens],
	get_values_for_key(Json, KeysList);
get_values(Json, Key) when is_binary(Key) ->
	get_values(Json, binary_to_list(Key)).

%% @doc Gets the first JSON child documents that contains a key
%%      that maps to a specified value.
%%
%% @spec select_first_value(jsondoc(), {string(), term()}) -> jsondoc()    
%% @end
select_first_value(Json, {Key, Value}) ->
	case select_values(Json, {Key, Value}) of
		[] ->
			undefined;
		[Document|_Rest] ->
			Document
	end.

%% @doc Gets all JSON child documents that contain a key
%%      that maps to a specified value.
%%
%% @spec select_values(jsondoc(), {string(), term()}) -> [jsondoc()]    
%% @end
select_values({ok, Result}=_Json, {Key, Value}) ->
	select_values(Result, {Key, Value});
select_values(Json, {Key, Value}) when is_list(Key) ->
	Tokens = string:tokens(Key, "."),
	KeysList = [list_to_binary(Token) || Token <- Tokens],
	select_values_for_key(Json, {KeysList, Value}, []);
select_values(Document, {Key, Value}) when is_binary(Key) ->
	select_values(Document, {binary_to_list(Key), Value}).


%% Local Functions
get_values_for_key(Doc, []) ->
	Doc;
get_values_for_key(Doc, [Key|Tail]) ->
	UpdatedResult = get_values_for_key(Doc, Key, []),
	get_values_for_key(UpdatedResult, Tail).

get_values_for_key(Doc, Key, Result) when is_list(Key) ->
	get_values_for_key(Doc, list_to_binary(Key), Result);
get_values_for_key([Doc|Tail], Key, Result) when is_list(Doc) ->
	get_values_for_key(Doc, Key, Result) ++ get_values_for_key(Tail, Key, Result);
get_values_for_key({[]}, _Key, Result) ->
	lists:reverse(Result);
get_values_for_key({[{Key, Value}|Tail]}, Key, Result) ->
	get_values_for_key({Tail}, Key, [Value|Result]);
get_values_for_key({[{_NotTheKey, _Value}|Tail]}, Key, Result) ->
	get_values_for_key({Tail}, Key, Result);
get_values_for_key([], _Key, Result) ->
	lists:reverse(Result);
get_values_for_key([ {[{Key, Value}|DocTail]}|ListTail], Key, Result) ->
	get_values_for_key([ {DocTail}|ListTail ], Key, [Value|Result]);
get_values_for_key([ {[{_NotTheKey, _Value}|DocTail]}|ListTail], Key, Result) ->
	get_values_for_key([ {DocTail}|ListTail ], Key, Result);
get_values_for_key([_Value|Tail], Key, Result) ->
	get_values_for_key(Tail, Key, Result).

select_values_for_key(Doc, {[Key], Value}) ->
	case get_values_for_key(Doc, [Key]) of
		[Value] ->
			[Doc];
		_ ->
			[]
	end;
select_values_for_key(Doc, {[Key|_KeyTail], _Value}) ->
	get_values_for_key(Doc, [Key]).

select_values_for_key(Doc, KeyValue, []) when not is_list(Doc) ->
	select_values_for_key([Doc], KeyValue, []);
select_values_for_key([], {[_Key], _Value}, Result) ->
	Result;
select_values_for_key([], {[_Key|KeyTail], Value}, Result) ->
	select_values_for_key(Result, {KeyTail, Value}, []);
select_values_for_key([Doc|DocTail], {[Key|KeyTail], Value}, Result) ->
	DocResults = select_values_for_key(Doc, {[Key|KeyTail], Value}),
	DR2 = case DocResults of
			  [NestedList] ->
				  case is_list(NestedList) of
					  true ->
						  NestedList;
					  false ->
						  DocResults
				  end;
			  _ ->
				  DocResults
		  end,
	select_values_for_key(DocTail, {[Key|KeyTail], Value}, Result ++ DR2).
