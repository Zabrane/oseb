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
%% @doc Functions for interacting with the OpenStack Keystone service. <br/>
%%
%%      This module is used to authenticate a user against the Keystone service, 
%%      to access authentication tokens and to locate service endpoints.
%% @end     

-module(os_keystone).

-behaviour(gen_server).

%% Includes
-include("os_keystone.hrl").

%% API
-export([clone/1,
		 close/1,
		 get_auth_token/1,
		 get_endpoint/1,
		 get_extension_details/2,
		 get_public_url/2,
		 get_service_catalog/1,
		 get_tenant_id/1,
		 list_extensions/1,
		 list_public_urls/2,
		 list_tenants/1,
		 start/2,
		 start/4]).

%% gen_server callbacks
-export([init/1, 
		 handle_call/3, 
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% Records
-record(state, {endpoint, body, credentials}).

%% API
%% @doc Spawns a clone of a Keystone process with the same state as the original
%%      process. Most services need access to Keystone, if one service crashes
%%      a Keystone process, unrelated services should not crash as a result.
%%      The service processes (e.g. Nova, Glance, etc.) clone the Keystone
%%      process and interact with the clone.
%%
%% @spec clone(pid()) -> {ok, pid()}    
%% @end
clone(Pid) ->
	{ok, State} = gen_server:call(Pid, get_state, infinity),
	gen_server:start(?MODULE, [State], [{timeout, infinity}]).
	
%% @doc Closes the process.
%%
%% @spec close(pid()) -> ok    
%% @end
close(Pid) ->
	gen_server:call(Pid, close, infinity).
	
%% @doc Gets the authentication token to be sent in requests to OpenStack
%%      services.
%%
%% @spec get_auth_token(pid()) -> {ok, string()}    
%% @end
get_auth_token(Pid) ->
	gen_server:call(Pid, get_auth_token, infinity).

%% @doc Gets the Keystone service endpoint.
%%
%% @spec get_endpoint(pid()) -> {ok, string()}    
%% @end
get_endpoint(Pid) ->
	gen_server:call(Pid, get_endpoint, infinity).

%% @doc Gets the details about an extension to the Keystone API.
%%
%% @spec get_extension_details(pid(), string()) -> {ok, jsondoc()}    
%% @end
get_extension_details(Pid, Alias) ->
	gen_server:call(Pid, {get_extension_details, Alias}, infinity).

%% @doc Gets the first public URL in the service catalog for a service
%%      of a specified type.
%%
%% @spec get_public_url(pid(), string()) -> {ok, string()}    
%% @end
get_public_url(Pid, ServiceType) when is_list(ServiceType) ->
	get_public_url(Pid, list_to_binary(ServiceType));
get_public_url(Pid, ServiceType) ->
	gen_server:call(Pid, {get_public_url, ServiceType}, infinity).
	
%% @doc Gets the service catalog.
%%
%% @spec get_service_catalog(pid()) -> {ok, jsondoc()}    
%% @end
get_service_catalog(Pid) ->
	gen_server:call(Pid, get_service_catalog, infinity).

%% @doc Gets the tenant ID.
%%
%% @spec get_tenant_id(pid()) -> {ok, string()}    
%% @end
get_tenant_id(Pid) ->
	gen_server:call(Pid, get_tenant_id, infinity).

%% @doc Lists supported extensions to the Keystone service.
%%
%% @spec list_extensions(pid()) -> {ok, jsondoc()}    
%% @end
list_extensions(Pid) ->
	gen_server:call(Pid, list_extensions, infinity).

%% @doc Lists public URLs for a specified service type.
%%
%% @spec list_public_urls(pid(), string()) -> {ok, list(string())}    
%% @end
list_public_urls(Pid, ServiceType) when is_list(ServiceType) ->
	list_public_urls(Pid, list_to_binary(ServiceType));
list_public_urls(Pid, ServiceType) ->
	gen_server:call(Pid, {get_public_urls, ServiceType}, infinity).
	
%% @doc Lists all tenants.
%%
%% @spec list_tenants(pid()) -> {ok, jsondoc()}    
%% @end
list_tenants(Pid) ->
	gen_server:call(Pid, list_tenants, infinity).

%% @doc Spawns a process for interacting with the Keystone service if
%%      the supplied credentials can be validated.
%%
%% @spec start(url(), term()) -> {ok, pid()}    
%% @end
start(Endpoint, Credentials) when is_list(Credentials) ->
	Url = openstack_rest:flatten_iolist([Endpoint, "/tokens"]),
	case httpc:request(post, 
					   {Url, [{"accept", "application/json"}], "application/json", Credentials},
					   [], 
					   [{full_result, false}, {body_format, binary}]) of
		{ok, {200, Body}} ->
			JsonBody = json_eep:json_to_term(binary_to_list(Body)),
			gen_server:start(?MODULE, [#state{body=JsonBody, endpoint=Endpoint}], [{timeout, infinity}]);
		{ok, {Code, Body}} ->
			{error, {Code, Body}};
		{error, Reason} ->
			{error, Reason};
		OtherError ->
			{error, OtherError}
	end;
start(Endpoint, {CredentialsList}=Credentials) when is_list(CredentialsList) ->
	start(Endpoint, json_eep:term_to_json(Credentials));
start(Endpoint, Credentials) when is_tuple(Credentials) ->
	start(Endpoint, openstack_records:map(Credentials)).

%% @doc Spawns a process for interacting with the Keystone service if
%%      the supplied password can be verified for the associated
%%      user and tenant.
%%
%% @spec start(url(), string(), string(), string()) -> {ok, pid()}    
%% @end
start(Endpoint, UserName, Password, TenantName) when is_list(UserName) ->
	start(Endpoint, list_to_binary(UserName), Password, TenantName);
start(Endpoint, UserName, Password, TenantName) when is_list(Password) ->
	start(Endpoint, UserName, list_to_binary(Password), TenantName);
start(Endpoint, UserName, Password, TenantName) when is_list(TenantName) ->
	start(Endpoint, UserName, Password, list_to_binary(TenantName));
start(Endpoint, UserName, Password, TenantName) ->
	PasswordCredentials = #passwordCredentials{password=Password, username=UserName},
	Auth = #auth{passwordCredentials=PasswordCredentials, tenantName=TenantName},
	start(Endpoint, Auth).


%% Server functions

%% @private
%% @doc Initializes the process.
init([State]) ->
	{ok, State}.

%% @private
%% @doc Handles synchronous calls.
handle_call(close, _From, #state{}=State) ->
	{stop, normal, ok, State};
handle_call(get_auth_token, _From, #state{}=State) ->
	{reply, {ok, get_auth_token_as_string(State)}, State};
handle_call(get_endpoint, _From, State) ->
	{reply, {ok, State#state.endpoint}, #state{}=State};
handle_call({get_extension_details, Alias}, _From, #state{}=State) ->
	XAuthToken = get_auth_token_as_string(State), 
	Reply = openstack_rest:get_json(State#state.endpoint, ["extensions/", Alias], XAuthToken),
 	{reply, Reply, State};
handle_call({get_public_url, ServiceType}, _From, #state{}=State) ->
	Services = openstack_json:get_first_value(State#state.body, "access.serviceCatalog"),
	Service = openstack_json:select_first_value(Services, {<<"type">>, ServiceType}),
	Reply = case Service of
				undefined ->
					{error, undefined};
				Service ->
					{ok, openstack_json:get_first_value(Service, "endpoints.publicURL")}
			end,
	{reply, Reply, State};
handle_call({get_public_urls, ServiceType}, _From, #state{}=State) ->
	Services = openstack_json:get_first_value(State#state.body, "access.serviceCatalog"),
	Service = openstack_json:select_first_value(Services, {<<"type">>, ServiceType}),
	Reply = case Service of
				undefined ->
					[];
				Service ->
					openstack_json:get_values(Service, "endpoints.publicURL")
			end,
	{reply, {ok, Reply}, State};
handle_call(get_service_catalog, _From, #state{}=State) ->
	{reply, {ok, openstack_json:get_first_value(State#state.body, "access.serviceCatalog")}, State};
handle_call(get_state, _From, #state{}=State) ->
	{reply, {ok, State}, State};
handle_call(get_tenant_id, _From, #state{}=State) ->
	TenantId = openstack_json:get_first_value(State#state.body, "access.token.tenant.id"),
	{reply, {ok, TenantId}, State};
handle_call(list_extensions, _From, #state{}=State) ->
	XAuthToken = get_auth_token_as_string(State), 
	Reply = openstack_rest:get_json(State#state.endpoint, "extensions", XAuthToken),
 	{reply, Reply, State};
handle_call(list_tenants, _From, #state{}=State) ->
	XAuthToken = get_auth_token_as_string(State), 
	Reply = openstack_rest:get_json(State#state.endpoint, "tenants", XAuthToken),
 	{reply, Reply, State};
handle_call(Message, _From, State) ->
	{reply, {error, {bad_message, Message}}, State}.

%% @private
%% @doc Handles asynchronous calls.
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
%% @doc Handles out of band messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Cleans up when the process terminates.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc Responds to updates of the code.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%% Local functions.
%% The auth token is stored as a binary, it's more convenient to get it as a string.
get_auth_token_as_string(State) ->
	XAuthToken = openstack_json:get_first_value(State#state.body, "access.token.id"), 
	binary_to_list(XAuthToken).
