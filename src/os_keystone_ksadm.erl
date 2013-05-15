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
%% @doc Functions for interacting with the administrative extensions to the
%%      OpenStack Keystone service. <br/>
%%
%%      This module is used to administer users, roles, services and tenants.
%% @end     
-module(os_keystone_ksadm).

%% Include files
-include("os_keystone.hrl").

%% External exports
-export([add_role/2,
		 add_role_to_tenant_user/4,
		 add_service/2,
		 add_tenant/2,
		 add_user/2,
		 delete_role/2,
		 delete_role_from_tenant_user/4,
		 delete_service/2,
		 delete_tenant/2,
		 delete_user/2,
		 get_role/2,
		 get_service/2,
		 list_roles/1,
		 list_roles/2,
		 list_services/1,
		 list_services/2,
		 list_users/1,
		 update_tenant/3,
		 update_user/3]).

%% API
%% @doc Creates a new role.
%%
%% @spec add_role(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
add_role(KeystonePid, Role) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:post_json(Endpoint, "OS-KSADM/roles", XAuthToken, Role).

%% @doc Associates a user with a specified role in a tenant.
%%
%% @spec add_role_to_tenant_user(pid(), id(), id(), id()) -> {ok, jsondoc()}    
%% @end
add_role_to_tenant_user(KeystonePid, TenantId, UserId, RoleId) when is_integer(TenantId) ->
	add_role_to_tenant_user(KeystonePid, integer_to_list(TenantId), UserId, RoleId);
add_role_to_tenant_user(KeystonePid, TenantId, UserId, RoleId) when is_integer(UserId) ->
	add_role_to_tenant_user(KeystonePid, TenantId, integer_to_list(UserId), RoleId);
add_role_to_tenant_user(KeystonePid, TenantId, UserId, RoleId) when is_integer(RoleId) ->
	add_role_to_tenant_user(KeystonePid, TenantId, UserId, integer_to_list(RoleId));
add_role_to_tenant_user(KeystonePid, TenantId, UserId, RoleId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:put(Endpoint, ["tenants/", TenantId, "/users/", UserId, "/roles/OS-KSADM/", RoleId], 
				XAuthToken, [{"accept", "application/json"}], "application/json", <<"null">>).
	
%% @doc Creates a new tenant.
%%
%% @spec add_tenant(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
add_tenant(KeystonePid, Tenant) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:post_json(Endpoint, "tenants", XAuthToken, Tenant).

%% @doc Creates a new service.
%%
%% @spec add_service(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
add_service(KeystonePid, Service) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:post_json(Endpoint, "OS-KSADM/services", XAuthToken, Service).

%% @doc Creates a new user.
%%
%% @spec add_user(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
add_user(KeystonePid, User) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	UserJson = openstack_records:map_to_json(User),
	openstack_rest:post_json(Endpoint, "users", XAuthToken, UserJson).

%% @doc Deletes a role.
%%
%% @spec delete_role(pid(), id()) -> ok    
%% @end
delete_role(KeystonePid, RoleId) when is_integer(RoleId) ->
	delete_role(KeystonePid, integer_to_list(RoleId));
delete_role(KeystonePid, RoleId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:delete_json(Endpoint, ["OS-KSADM/roles/", RoleId], XAuthToken).

%% @doc Removes a role from a user in a specific tenant.
%%
%% @spec delete_role_from_tenant_user(pid(), id(), id(), id()) -> ok    
%% @end
delete_role_from_tenant_user(KeystonePid, TenantId, UserId, RoleId) when is_integer(TenantId) ->
	delete_role_from_tenant_user(KeystonePid, integer_to_list(TenantId), UserId, RoleId);
delete_role_from_tenant_user(KeystonePid, TenantId, UserId, RoleId) when is_integer(UserId) ->
	delete_role_from_tenant_user(KeystonePid, TenantId, integer_to_list(UserId), RoleId);
delete_role_from_tenant_user(KeystonePid, TenantId, UserId, RoleId) when is_integer(RoleId) ->
	delete_role_from_tenant_user(KeystonePid, TenantId, UserId, integer_to_list(RoleId));
delete_role_from_tenant_user(KeystonePid, TenantId, UserId, RoleId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:delete_json(Endpoint, 
						["tenants/", TenantId, "/users/", UserId, "/roles/OS-KSADM/", RoleId], 
						XAuthToken).
	
%% @doc Deletes a service.
%%
%% @spec delete_service(pid(), id()) -> ok    
%% @end
delete_service(KeystonePid, ServiceId) when is_integer(ServiceId) ->
	delete_service(KeystonePid, integer_to_list(ServiceId));
delete_service(KeystonePid, ServiceId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:delete_json(Endpoint, ["OS-KSADM/services/", ServiceId], XAuthToken).

%% @doc Deletes a tenant.
%%
%% @spec delete_tenant(pid(), id()) -> ok    
%% @end
delete_tenant(KeystonePid, TenantId) when is_integer(TenantId) ->
	delete_tenant(KeystonePid, integer_to_list(TenantId));
delete_tenant(KeystonePid, TenantId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:delete_json(Endpoint, ["tenants/", TenantId], XAuthToken).

%% @doc Deletes a user.
%%
%% @spec delete_user(pid(), id()) -> ok    
%% @end
delete_user(KeystonePid, UserId) when is_integer(UserId) ->
	delete_user(KeystonePid, integer_to_list(UserId));
delete_user(KeystonePid, UserId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:delete_json(Endpoint, ["users/", UserId], XAuthToken).
	
%% @doc Gets the details of a role for a specified ID.
%%
%% @spec get_role(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_role(KeystonePid, RoleId) when is_integer(RoleId) ->
	get_role(KeystonePid, integer_to_list(RoleId));
get_role(KeystonePid, RoleId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, ["OS-KSADM/roles/", RoleId], XAuthToken).

%% @doc Gets the details of a service for a specified ID.
%%
%% @spec get_service(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_service(KeystonePid, ServiceId) when is_integer(ServiceId) ->
	get_service(KeystonePid, integer_to_list(ServiceId));
get_service(KeystonePid, ServiceId) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, ["OS-KSADM/services/", ServiceId], XAuthToken).

%% @doc Lists all roles.
%%
%% @spec list_roles(pid()) -> {ok, jsondoc()}    
%% @end
list_roles(KeystonePid) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, "OS-KSADM/roles", XAuthToken).
	
%% @doc Lists all roles satisfying a filter and/or limit criteria.
%%
%% @spec list_roles(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_roles(KeystonePid, Filter) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, ["OS-KSADM/roles?", openstack_records:map_filter(Filter)], XAuthToken).
	
%% @doc Lists all services.
%%
%% @spec list_services(pid()) -> {ok, jsondoc()}    
%% @end
list_services(KeystonePid) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, "OS-KSADM/services", XAuthToken).
	
%% @doc Lists all services satisfying a filter and/or limit criteria.
%%
%% @spec list_services(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_services(KeystonePid, Filter) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, ["OS-KSADM/services?", openstack_records:map_filter(Filter)], XAuthToken).
	
%% @doc Lists all users.
%%
%% @spec list_users(pid()) -> {ok, jsondoc()}    
%% @end
list_users(KeystonePid) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	openstack_rest:get_json(Endpoint, "users", XAuthToken).
 
%% @doc Updates the details of a tenant.
%%
%% @spec update_tenant(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
update_tenant(KeystonePid, TenantId, Tenant) when is_integer(TenantId) ->
	update_tenant(KeystonePid, integer_to_list(TenantId), Tenant);
update_tenant(KeystonePid, TenantId, Tenant) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	TenantJson = openstack_records:map_to_json(Tenant),
	openstack_rest:put_json(Endpoint, ["tenants/", TenantId], XAuthToken, TenantJson).

%% @doc Updates the details of a user.
%%
%% @spec update_user(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
update_user(KeystonePid, UserId, User) when is_integer(UserId) ->
	update_user(KeystonePid, integer_to_list(UserId), User);
update_user(KeystonePid, UserId, User) ->
	{XAuthToken, Endpoint} = get_token_endpoint(KeystonePid),
	UserJson = openstack_records:map_to_json(User),
	openstack_rest:put_json(Endpoint, ["users/", UserId], XAuthToken, UserJson).


%% Local functions
%% Returns the authentication token and the Keystone service enpoint.
get_token_endpoint(KeystonePid) ->
	{ok, XAuthToken} = os_keystone:get_auth_token(KeystonePid),
	{ok, Endpoint} = os_keystone:get_endpoint(KeystonePid),
	{XAuthToken, Endpoint}.
