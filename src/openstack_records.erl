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

%%% @author CA Meijer
%%% @copyright 2013 CA Meijer
%%% @doc Openstack server. This module exports functions to map records
%%%      to documents and to map documents to records.
%%% @end

-module(openstack_records).

-behaviour(gen_server).

%% API
-export([start_link/1, 
		 add_mapping/1,
		 add_mappings/1,
		 filter_headers/2,
		 get_mapping/1,
		 is_mapped/1,
		 map/1,
		 map_to_json/1,
		 map_filter/1,
		 map_headers/2,
		 unmap_headers/3]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

-define(SERVER, ?MODULE).

%% We store the ETS table ID across calls.
-record(state, {ets_table_id}).

%% External functions

%% @doc Spawns a registered process on the local node that stores the structure of records in an ETS table.
%%
%% @spec start_link(integer()) -> {ok, pid()}    
%% @end
start_link(EtsTableId) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [EtsTableId], []).

%% @doc Specfies the field identifiers associated with a record name.
%% @spec add_mapping({atom(), list(atom())}) -> ok    
%% @end
add_mapping({RecordName, FieldIds} = RecordDescriptor) ->
	assert_is_mapping(RecordDescriptor),
	server_call(add_mapping, {RecordName, FieldIds}).

%% @doc Specfies the field identifiers associated with a list of record names.
%% @spec add_mappings(list({atom(), list(atom())})) -> ok    
%% @end
add_mappings(RecordDescriptorList) ->
	[assert_is_mapping(Mapping) || Mapping <- RecordDescriptorList],
	[add_mapping(Mapping) || Mapping <- RecordDescriptorList],
	ok.

%% @doc Filters HTTP headers that start with a specified prefix.
%% @spec filter_headers(string(), proplist()) -> record()
filter_headers(HeaderPrefix, Headers) ->
	HeaderPrefixLower = string:to_lower(HeaderPrefix),
	[{Key, Value} || {Key, Value} <-Headers,
					 string:sub_string(string:to_lower(Key), 
									   1, 
									   length(HeaderPrefixLower)) =:= HeaderPrefixLower].

%% @doc Gets the field names associated with a record.
%% @spec get_mapping(record()) -> list(atom())
get_mapping(Record) ->
	[Tag|_Tail] = tuple_to_list(Record),
	server_call(get_mapping, Tag).
	
%% @doc Gets the field names associated with a record.
%% @spec is_mapped(record()|atom()) -> true | false
is_mapped(Value) ->
	server_call(is_mapped, Value).

%% @doc Maps a record to an eep_json encoding of a JSON document.
%% @spec map(record()) -> jsondoc()
map(Value) ->
	case is_mapped(Value) of
		true ->
			map_record(Value);
		false ->
			case is_list(Value) of
				false ->
					Value;
				true ->
					map_list(Value)
			end
	end.

%% @doc Maps a record to a URL encoded query string.
%% @spec map_filter(record()) -> string()
map_filter(Record) when is_tuple(Record) ->
	[_Tag|Values] = tuple_to_list(Record),
	FieldIds = get_mapping(Record),
	map_filter(FieldIds, Values, "");
map_filter(Filter) when is_list(Filter) ->
	Filter;
map_filter(Filter) when is_binary(Filter) ->
	binary_to_list(Filter).

%% @doc Maps a record to key/value pairs for an HTTP header.
%% @spec map_headers(string(), record()) -> proplist()
map_headers(HeaderPrefix, Record) ->
	[_Tag|Values] = tuple_to_list(Record),
	FieldIds = get_mapping(Record),
	map_headers(HeaderPrefix, FieldIds, Values, []).

%% @doc Maps a record to a JSON document.
%% @spec map_to_json(record()) -> string()
map_to_json(String) when is_list(String) ->
	String;
map_to_json({Terms}) when is_list(Terms) ->
	json_eep:term_to_json({Terms});
map_to_json(Record) when is_tuple(Record) ->
	MappedRecord = map(Record),
	json_eep:term_to_json(MappedRecord).

%% @doc Maps HTTP headers to a record.
%% @spec unmap_headers(atom(), string(), proplist()) -> record()
unmap_headers(RecordTag, HeaderPrefix, Headers) ->
	FieldIds = 	server_call(get_mapping, RecordTag),
	FieldIdsStr = [string:to_lower(atom_to_list(FieldId)) || FieldId <- FieldIds],
	RecordList = unmap_headers(FieldIdsStr, HeaderPrefix, Headers, [undefined || _FieldId <- FieldIds]),
	list_to_tuple([RecordTag] ++ RecordList).


%% Server functions

%% @private
%% @doc Initializes the server with the ETS table used to store the
%%      mappings needed for mapping records to documents.
-spec(init(EtsTableId::list(integer())) -> {ok, #state{}}).
init([EtsTableId]) ->
	{ok, #state{ets_table_id = EtsTableId}}.

%% @private
%% @doc Responds synchronously to server calls. This function is invoked when a mapping is
%%      added or a mapping needs to be retrieved.
-spec(handle_call(Message::tuple(), From::pid(), State::#state{}) -> {reply, Reply::any(), NewState::record()}).
handle_call({add_mapping, {Key, Value}}, _From, State) ->
	true = ets:insert(State#state.ets_table_id, {Key, Value}),
	{reply, ok, State};
handle_call({get_mapping, Tag}, _From, State) ->
	[{Tag, Fields}] = ets:lookup(State#state.ets_table_id, Tag),
	{reply, Fields, State};
handle_call({is_mapped, Value}, _From, State) when is_tuple(Value) ->
	[Tag|Tail] = tuple_to_list(Value),
	Reply = case ets:lookup(State#state.ets_table_id, Tag) of
		[] -> 
			false;
		[{Tag, Fields}] ->
			length(Tail) =:= length(Fields)
	end,
	{reply, Reply, State};
handle_call({is_mapped, _Value}, _From, State) ->
	{reply, false, State}.


%% @private
%% @doc Responds asynchronously to messages. Asynchronous messages are ignored.
-spec(handle_cast(any(), State::#state{}) -> {no_reply, State::#state{}}).
handle_cast(_Message, State) ->
	{noreply, State}.

%% @private
%% @doc Responds to non-OTP messages. Non-OTP messages are ignored.
-spec(handle_info(any(), State::#state{}) -> {no_reply, State::#state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
%% @doc Handles the shutdown of the server.
-spec(terminate(any(), #state{}) -> ok).
terminate(_Reason, _State) ->
	ok.

%% @private
%% @doc Responds to code changes.
-spec(code_change(any(), State::#state{}, any()) -> {ok, State::#state{}}).
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%% Internal functions
server_call(Command, Args) ->
	gen_server:call(?SERVER, {Command, Args}, infinity).

assert_is_mapping({RecordName, FieldIds}) when is_atom(RecordName) ->
	[true = is_atom(FieldId) || FieldId <- FieldIds].

map_record(Record) ->
	[Tag|Values] = tuple_to_list(Record),
	FieldIds = get_mapping(Record),
	{[{atom_to_binary(Tag, utf8), {map_record_attrs(FieldIds, Values, [])}}]}.

map_record_attrs([], [], Result) ->
	Result;
map_record_attrs([_FieldId|FieldsTail], [undefined|ValuesTail], Result) ->
	map_record_attrs(FieldsTail, ValuesTail, Result);
map_record_attrs([FieldId|FieldsTail], [Value|ValuesTail], Result) ->
	case is_mapped(Value) of
		false ->
			map_record_attrs(FieldsTail, ValuesTail, Result ++ [{atom_to_binary(FieldId, utf8), map(Value)}]);
		true ->
			{[MappedValue]} = map(Value),
			map_record_attrs(FieldsTail, ValuesTail, Result ++ [MappedValue])			
	end.

map_list(List) ->
	map_list(List, []).

map_list([], Result) ->
	Result;
map_list([Head|Tail], Result) ->
	case is_mapped(Head) of
		false ->
			map_list(Tail, Result ++ [Head]);
		true ->
			map_list(Tail, Result ++ [map_record(Head)])
	end.

map_filter([], [], "") ->
	"";
map_filter([], [], Result) ->
	"&" ++ ResRev = lists:reverse(Result),
	lists:reverse(ResRev);
map_filter([_FieldId|FieldsTail], [undefined|ValuesTail], Result) ->
	map_filter(FieldsTail, ValuesTail, Result);
map_filter([FieldId|FieldsTail], [Value|ValuesTail], Result) when is_integer(Value) ->
	map_filter(FieldsTail, ValuesTail, Result ++ atom_to_list(FieldId) ++ "=" ++  integer_to_list(Value) ++ "&");
map_filter([FieldId|FieldsTail], [Value|ValuesTail], Result) when is_list(Value) ->
	map_filter(FieldsTail, ValuesTail, Result ++ atom_to_list(FieldId) ++ "=" ++ edoc_lib:escape_uri(Value) ++ "&");
map_filter([FieldId|FieldsTail], [Value|ValuesTail], Result) when is_binary(Value) ->
	map_filter(FieldsTail, ValuesTail, Result ++ atom_to_list(FieldId) ++ "=" ++ binary_to_list(Value) ++ "&").

map_headers(_HeaderPrefix, [], [], Result) ->
	Result;
map_headers(HeaderPrefix, [_FieldId|FieldsTail], [undefined|ValuesTail], Result) ->
	map_headers(HeaderPrefix, FieldsTail, ValuesTail, Result);
map_headers(HeaderPrefix, [FieldId|FieldsTail], [Value|ValuesTail], Result) when is_list(Value) ->
	map_headers(HeaderPrefix, FieldsTail, ValuesTail, Result ++ [{HeaderPrefix ++ atom_to_list(FieldId), Value}]);
map_headers(HeaderPrefix, [FieldId|FieldsTail], [Value|ValuesTail], Result) when is_integer(Value) ->
	map_headers(HeaderPrefix, [FieldId|FieldsTail], [integer_to_list(Value)|ValuesTail], Result);
map_headers(HeaderPrefix, [FieldId|FieldsTail], [Value|ValuesTail], Result) when is_binary(Value) ->
	map_headers(HeaderPrefix, [FieldId|FieldsTail], [binary_to_list(Value)|ValuesTail], Result).

set_field([], _Key, _Value, [], Result) ->
	Result;
set_field([Key|Tail], Key, Value, [_InitH|InitT], Result) ->
	set_field(Tail, Key, Value, InitT, Result ++ [list_to_binary(Value)]);
set_field([_FieldId|Tail], Key, Value, [InitH|InitT], Result) ->
	set_field(Tail, Key, Value, InitT, Result ++ [InitH]).

unmap_headers(_FieldIds, _HeaderPrefix, [], Result) ->
	Result;
unmap_headers(FieldIds, HeaderPrefix, [{HeaderKey, HeaderValue}|HeaderTail], Result) ->
	UpdatedResult = case string:to_lower(string:sub_string(HeaderKey, 1, length(HeaderPrefix))) of
		HeaderPrefix ->
			set_field(FieldIds, string:to_lower(string:sub_string(HeaderKey, length(HeaderPrefix) + 1)), 
					  HeaderValue, Result, []);
		_ ->
			Result
	end,
	unmap_headers(FieldIds, HeaderPrefix, HeaderTail, UpdatedResult).

