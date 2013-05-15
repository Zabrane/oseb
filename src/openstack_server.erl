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
%% @doc This module exposes CRUD operations that the various OpenStack services
%%      (Keystone, Nova, Glance, etc) need.
%% @end     

-module(openstack_server).

-behaviour(gen_server).

%% External exports
-export([close/1,
		 delete/2,
		 download/3,
		 get/2,
		 get_binary/2,
		 get_keystone_pid/1,
		 head/2,
		 post/2,
		 post/3,
		 post/4,
		 put/3,
		 put/4,
		 start/2,
		 upload_post/4,
		 upload_put/4]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% Records
-record(state, {keystone_pid, base_url}).

%% API
%% @doc Terminates the process.
%%
%% @spec close(pid()) -> ok    
%% @end
close(Pid) ->
	gen_server:call(Pid, close, infinity).

%% @doc Deletes a resource.
%%
%% @spec delete(pid(), iolist()) -> {ok, jsondoc()}    
%% @end
delete(Pid, RelativeUrl) ->
	gen_server:call(Pid, {delete, RelativeUrl}, infinity).

%% @doc Gets a resource and streams it to a file.
%%
%% @spec download(pid(), iolist(), string()) -> {ok, term()}    
%% @end
download(Pid, RelativeUrl, FileName) ->
	gen_server:call(Pid, {download, RelativeUrl, FileName}, infinity).

%% @doc Gets a resource as a JSON document.
%%
%% @spec get(pid(), iolist()) -> {ok, jsondoc()}    
%% @end
get(Pid, RelativeUrl) ->
	gen_server:call(Pid, {get, RelativeUrl}, infinity).

%% @doc Gets a resource as a binary value.
%%
%% @spec get_binary(pid(), iolist()) -> {ok, binary()}    
%% @end
get_binary(Pid, RelativeUrl) ->
	gen_server:call(Pid, {get_binary, RelativeUrl}, infinity).

%% @doc Gets a Keystone process identifier that was passed an initialization
%%      parameter to this server.
%%
%% @spec get_keystone_pid(pid()) -> {ok, pid()}    
%% @end
get_keystone_pid(Pid) ->
	gen_server:call(Pid, get_keystone_pid, infinity).
	
%% @doc Gets headers associated with a resource.
%%
%% @spec head(pid(), iolist()) -> {ok, list()}    
%% @end
head(Pid, RelativeUrl) ->
	gen_server:call(Pid, {head, RelativeUrl}, infinity).

%% @doc Creates a resource.
%%
%% @spec post(pid(), iolist()) -> {ok, jsondoc()}    
%% @end
post(Pid, RelativeUrl) ->
	post(Pid, RelativeUrl, "{}").

%% @doc Creates a resource using the specified initialization JSON document.
%%
%% @spec post(pid(), iolist(), jsondoc()) -> {ok, jsondoc()}    
%% @end
post(Pid, RelativeUrl, Body) ->
	gen_server:call(Pid, {post, RelativeUrl, Body}, infinity).

%% @doc Creates a resource using the specified headers and binary data.
%%
%% @spec post(pid(), iolist(), proplist(), binary()) -> {ok, term()}    
%% @end
post(Pid, RelativeUrl, Headers, BinaryData) ->
	gen_server:call(Pid, {post, RelativeUrl, Headers, BinaryData}, infinity).

%% @doc Updates a resource.
%%
%% @spec put(pid(), iolist(), jsondoc()) -> {ok, jsondoc()}    
%% @end
put(Pid, RelativeUrl, Body) ->
	gen_server:call(Pid, {put, RelativeUrl, Body}, infinity).

%% @doc Creates a resource using the specified headers and binary data.
%%
%% @spec put(pid(), iolist(), proplist(), binary()) -> {ok, term()}    
%% @end
put(Pid, RelativeUrl, Headers, BinaryData) ->
	gen_server:call(Pid, {put, RelativeUrl, Headers, BinaryData}, infinity).

%% @doc Starts a server using a Keystone process and an endpoint for the service
%%      to be started.
%%
%% @spec start(pid(), url()) -> {ok, pid()}    
%% @end
start(KeystonePid, BaseUrl) ->
	UrlList = openstack_rest:flatten_iolist(BaseUrl),
	Url = case lists:reverse(UrlList) of
			  [$/|Tail] ->
				  lists:reverse(Tail);
			  _ ->
				  BaseUrl
		  end,
	{ok, KeystoneClone} = os_keystone:clone(KeystonePid),
	State = #state{keystone_pid=KeystoneClone, base_url=Url},
	gen_server:start(?MODULE, [State], [{timeout, infinity}]).
	
%% @doc Creates a resource using initialization data to be read from
%%      a file.
%%
%% @spec upload_post(pid(), iolist(), proplist(), string()) -> {ok, term()}    
%% @end
upload_post(Pid, RelativeUrl, Headers, FileName) ->
	gen_server:call(Pid, {upload_post, RelativeUrl, Headers, FileName}, infinity).

%% @doc Creates or updates a resource using data read from a file.
%%
%% @spec upload_put(pid(), iolist(), proplist(), string()) -> {ok, term()}    
%% @end
upload_put(Pid, RelativeUrl, Headers, FileName) ->
	gen_server:call(Pid, {upload_put, RelativeUrl, Headers, FileName}, infinity).


%% Server functions
%% @private
init([State]) ->
	{ok, State}.

%% @private
handle_call(close, _From, State) ->
	{stop, normal, ok, State};
handle_call({delete, RelativeUrl}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:delete_json(State#state.base_url, RelativeUrl, XAuthToken),
 	{reply, Reply, State};
handle_call({download, RelativeUrl, FileName}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:download(State#state.base_url, RelativeUrl, XAuthToken, FileName),
 	{reply, Reply, State};
handle_call({get, RelativeUrl}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:get_json(State#state.base_url, RelativeUrl, XAuthToken),
 	{reply, Reply, State};
handle_call({get_binary, RelativeUrl}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:get_binary(State#state.base_url, RelativeUrl, XAuthToken),
 	{reply, Reply, State};
handle_call(get_keystone_pid, _From, State) ->
	{reply, State#state.keystone_pid, State};
handle_call({head, RelativeUrl}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:head(State#state.base_url, RelativeUrl, XAuthToken),
 	{reply, Reply, State};
handle_call({post, RelativeUrl, Body}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:post_json(State#state.base_url, RelativeUrl, XAuthToken, Body),
 	{reply, Reply, State};
handle_call({post, RelativeUrl, Headers, BinaryData}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	ContentType = openstack_rest:get_content_type(Headers, "application/octet-stream"),
	Reply = openstack_rest:post(State#state.base_url, RelativeUrl, XAuthToken, Headers, ContentType, BinaryData),
 	{reply, Reply, State};
handle_call({put, RelativeUrl, Body}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	Reply = openstack_rest:put_json(State#state.base_url, RelativeUrl, XAuthToken, Body),
 	{reply, Reply, State};
handle_call({put, RelativeUrl, Headers, BinaryData}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	ContentType = openstack_rest:get_content_type(Headers, "application/octet-stream"),
	Reply = openstack_rest:put(State#state.base_url, RelativeUrl, XAuthToken, Headers, ContentType, BinaryData),
 	{reply, Reply, State};
handle_call({upload_post, RelativeUrl, Headers, FileName}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	ContentType = openstack_rest:get_content_type(Headers, "application/octet-stream"),
	Reply = openstack_rest:upload_post(State#state.base_url, RelativeUrl, XAuthToken, Headers, ContentType, FileName),
 	{reply, Reply, State};
handle_call({upload_put, RelativeUrl, Headers, FileName}, _From, State) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(State#state.keystone_pid),
	ContentType = openstack_rest:get_content_type(Headers, "application/octet-stream"),
	Reply = openstack_rest:upload_put(State#state.base_url, RelativeUrl, XAuthToken, Headers, ContentType, FileName),
 	{reply, Reply, State}.

%% @private
handle_cast(_Message, State) ->
    {stop, cant_handle_asynchronous_calls, State}.

%% @private
handle_info(_Info, State) ->
    {stop, unexpected_message, State}.

%% @private
terminate(_Reason, State) ->
	ok = os_keystone:close(State#state.keystone_pid),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
