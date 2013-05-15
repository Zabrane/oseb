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
%% @doc Functions for handling HTTP request methods and the CRUD operations of REST.
%%      
-module(openstack_rest).

%% Include files
-include_lib("kernel/include/file.hrl").

-define(CHUNK_SIZE, 1048576).

%% External exports
-export([delete/4,
		 delete_json/3,
		 download/4,
		 flatten_iolist/1,
		 format_response/4,
		 get/4,
		 get_binary/3,
		 get_content_type/2,
		 get_json/3,
		 head/3,
		 post/6,
		 post_json/4,
		 put/6,
		 put_json/4,
		 upload_post/6,
		 upload_put/6]).


%% API
%% @doc Deletes a resource.
%%
%% @spec delete(string(), iolist(), string(), proplist()) -> ok    
%% @end
delete(BaseUrl, RelativeUrl, XAuthToken, Headers) ->
	Url = [BaseUrl, $/, RelativeUrl],
	request(delete, {Url, [{"x-auth-token", XAuthToken}|Headers]}).
	
%% @doc Deletes a resource.
%%
%% @spec delete_json(string(), iolist(), string()) -> ok    
%% @end
delete_json(BaseUrl, RelativeUrl, XAuthToken) ->
	delete(BaseUrl, RelativeUrl, XAuthToken, [{"accept", "application/json"}]).

%% @doc Performs an HTTP GET for an "octet-binary" application type and streams the result to
%%      a file.
%%
%% @spec download(string(), iolist(), string(), string()) -> ok    
%% @end
download(BaseUrl, RelativeUrl, XAuthToken, FileName) ->
	Url = flatten_iolist([BaseUrl, $/, RelativeUrl]),
	httpc:request(get, 
				  {Url, [{"x-auth-token", XAuthToken}, {"accept", "application/octet-stream"}]},
				  [],
				  [{body_format, binary}, {stream, FileName}]).

%% @doc A convenience function for parsing HTTP response codes. HTTP codes of the form 2XX
%%      are regarded as successes and other codes are interpreted as errors.
%%
%% @spec format_response(integer(), string(), proplist(), term()) -> ok | {ok, term()} | {error, term()}    
%% @end
format_response(204, _Phrase, _Headers, _Response) ->
	ok;
format_response(Code, _Phrase, Headers, Response) when Code div 100 =:= 2 ->
	ResponseData = case contains_json(Headers) of
					   true ->
						   JsonStr = binary_to_list(Response),
						   json_eep:json_to_term(JsonStr);
					   false ->
						   Response
				   end,
	{ok, ResponseData};
format_response(Code, Phrase, Headers, Response) ->
	ResponseData = case contains_json(Headers) of
					   true ->
						   JsonStr = binary_to_list(Response),
						   json_eep:json_to_term(JsonStr);
					   false ->
						   Phrase
				   end,
	{error, {Code, ResponseData}}.

%% @doc A convenience function that converts an iolist to a string.
%%
%% @spec flatten_iolist(iolist()) -> string()    
%% @end
flatten_iolist(IoList) ->
	binary_to_list((iolist_to_binary(IoList))).

%% @doc Gets a resource.
%%
%% @spec get(string(), iolist(), string(), proplist()) -> {ok, term()} | {error, term()}    
%% @end
get(BaseUrl, RelativeUrl, XAuthToken, Headers) ->
	Url = [BaseUrl, $/, RelativeUrl],
	request(get, {Url, [{"x-auth-token", XAuthToken}|Headers]}).

%% @doc Gets a resource.
%%
%% @spec get_binary(string(), iolist(), string()) -> {ok, binary()} | {error, term()}    
%% @end
get_binary(BaseUrl, RelativeUrl, XAuthToken) ->
	get(BaseUrl, RelativeUrl, XAuthToken, [{"accept", "application/octet-stream"}]).

%% @doc Extracts a content type from the headers or returns the default content
%%      type.
%%
%% @spec get_content_type(list(), string()) -> string()    
%% @end
get_content_type([], DefaultConentType) ->
	DefaultConentType;
get_content_type([{Key, Value}|Tail], DefaultConentType) ->
	case string:to_lower(Key) of
		"content-type" ->
			Value;
		_ ->
			get_content_type(Tail, DefaultConentType)
	end.

%% @doc Gets a resource.
%%
%% @spec get_json(string(), iolist(), string()) -> {ok, jsondoc()} | {error, term()}    
%% @end
get_json(BaseUrl, RelativeUrl, XAuthToken) ->
	get(BaseUrl, RelativeUrl, XAuthToken, [{"accept", "application/json"}]).

%% @doc Gets a resource.
%%
%% @spec head(string(), iolist(), string()) -> {ok, headers()} | {error, term()}    
%% @end
head(BaseUrl, RelativeUrl, XAuthToken) ->
	Url = [BaseUrl, $/, RelativeUrl],
	FlattenedUrlRequest = {flatten_iolist(Url), [{"x-auth-token", XAuthToken}]},
	{ok, {{_HttpVersion, Code, ReasonPhrase}, ResponseHeaders, _Response}} = 
		httpc:request(head, FlattenedUrlRequest, [], [{body_format, binary}]),
	case Code div 100 of 
		2 ->
			{ok, ResponseHeaders};
		_Error ->
			{error, {Code, ReasonPhrase}}
	end.

%% @doc Creates a resource.
%%
%% @spec post(string(), iolist(), string(), proplist(), string(), term()) -> {ok, term()} | {error, term()}    
%% @end
post(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, <<>>) ->
	put_post_no_content(post, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType);
post(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, "") ->
	put_post_no_content(post, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType);
post(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, Body) ->
	Url = [BaseUrl, $/, RelativeUrl],
	request(post, {Url, [{"x-auth-token", XAuthToken}] ++ Headers, ContentType, Body}).

%% @doc Creates a resource.
%%
%% @spec post_json(string(), iolist(), string(), jsondoc()) -> {ok, jsondoc()} | {error, term()}    
%% @end
post_json(BaseUrl, RelativeUrl, XAuthToken, Body) ->
	BodyJson = list_to_binary(openstack_records:map_to_json(Body)),
	post(BaseUrl, RelativeUrl, XAuthToken, [], "application/json", BodyJson).

%% @doc Updates a resource.
%%
%% @spec put(string(), iolist(), string(), proplist(), string(), term()) -> {ok, term()} | {error, term()}    
%% @end
put(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, <<>>) ->
	put_post_no_content(put, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType);
put(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, "") ->
	put_post_no_content(put, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType);
put(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, Body) ->
	Url = [BaseUrl, $/, RelativeUrl],
	request(put, {Url, [{"x-auth-token", XAuthToken}] ++ Headers, ContentType, Body}).

%% @doc Updates a resource.
%%
%% @spec put_json(string(), iolist(), string(), jsondoc()) -> {ok, jsondoc()} | {error, term()}    
%% @end
put_json(BaseUrl, RelativeUrl, XAuthToken, Body) ->
	BodyJson = list_to_binary(openstack_records:map_to_json(Body)),
	put(BaseUrl, RelativeUrl, XAuthToken, [], "application/json", BodyJson).

%% @doc Creates a resource by reading the material to POST from a file.
%%
%% @spec upload_post(string(), iolist(), string(), proplist(), string(), string()) -> {ok, term()} | {error, term()}    
%% @end
upload_post(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, FileName) ->
	upload(post, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, FileName).

%% @doc Creates or updates a resource by reading the material to POST from a file.
%%
%% @spec upload_put(string(), iolist(), string(), proplist(), string(), string()) -> {ok, term()} | {error, term()}    
%% @end
upload_put(BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, FileName) ->
	upload(put, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, FileName).

%% Local functions
request(Method, Request) ->
	FlattenedUrlRequest = case Request of
							  {Url, Headers} ->
								  {flatten_iolist(Url), Headers};
							  {Url, Headers, ContentType, Body} ->
								  {flatten_iolist(Url), Headers, ContentType, Body}
						  end,
	{ok, {{_HttpVersion, Code, ReasonPhrase}, ResponseHeaders, Response}} = 
		httpc:request(Method, FlattenedUrlRequest, [], [{body_format, binary}]),
	format_response(Code, ReasonPhrase, ResponseHeaders, Response).

contains_json([]) ->
	false;
contains_json([{Key, Value}|Tail]) ->
	case string:to_lower(Key) of
		"content-type" ->
			case string:to_lower(string:sub_string(Value, 1, length("application/json"))) of
				"application/json" ->
					true;
				_ ->
					contains_json(Tail)
			end;
		_ ->
			contains_json(Tail)
	end.

%% Returns a closure that can be used to read from a file merely by passing an index from which to
%% read.
get_file_reading_closure(FilePid, Size) ->
	fun(X) ->
			case X < Size of
				true ->
					{ok, Data} = file:pread(FilePid, X, ?CHUNK_SIZE),
					{ok, Data, X+?CHUNK_SIZE};
				false ->
					ok = file:close(FilePid),
					eof
			end
	end.

%% Fix for a bug in the httpc module that there is no "content-length" header
%% if the length is zero. Send the data chunked.
put_post_no_content(Method, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType) ->
	Url = [BaseUrl, $/, RelativeUrl],
	Body = {chunkify, fun(_X) -> eof end, 0},
	request(Method, {Url, [{"x-auth-token", XAuthToken}] ++ Headers, ContentType, Body}).

upload(Method, BaseUrl, RelativeUrl, XAuthToken, Headers, ContentType, FileName) ->
	Url = [BaseUrl, $/, RelativeUrl],
	case file:read_file_info(FileName) of
		{ok, FileInfo} ->
			Size = FileInfo#file_info.size,
			{ok, FilePid} = file:open(FileName, [read, binary]),
			Body = {chunkify, get_file_reading_closure(FilePid, Size), 0},
			request(Method, {Url, [{"x-auth-token", XAuthToken}] ++ Headers, ContentType, Body});
		Error ->
			Error
	end.

