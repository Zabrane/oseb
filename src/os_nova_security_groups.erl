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
%% @doc Functions for working with the security groups extensions to the
%%      Nova API.
%%      

-module(os_nova_security_groups).

%% Includes
-include("os_nova.hrl").

-export([add_group_to_server/3,
		 create/2,
		 create_rule/2,
		 delete/2,
		 delete_rule/2,
		 get/2,
		 list/1,
		 list_groups_for_server/2,
		 remove_group_from_server/3]).

%% API
%% @doc Adds a group to a server's list of security groups.
%%
%% @spec add_group_to_server(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
add_group_to_server(NovaPid, ServerId, GroupName) when is_list(GroupName) ->
	add_group_to_server(NovaPid, ServerId, list_to_binary(GroupName));
add_group_to_server(NovaPid, ServerId, GroupName) ->
	Action = #addSecurityGroup{name=GroupName},
	os_nova:action(NovaPid, ServerId, Action).

%% @doc Creates a new security group.
%%
%% @spec create(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create(NovaPid, SecurityGroupParameters) ->
	openstack_server:post(NovaPid, "os-security-groups", SecurityGroupParameters).

%% @doc Creates a new security group rule.
%%
%% @spec create_rule(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create_rule(NovaPid, SecurityGroupRuleParameters) ->
	openstack_server:post(NovaPid, "os-security-group-rules", SecurityGroupRuleParameters).

%% @doc Deletes a security group.
%%
%% @spec delete(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete(NovaPid, SecurityGroupId) when is_integer(SecurityGroupId) ->
	delete(NovaPid, integer_to_list(SecurityGroupId));
delete(NovaPid, SecurityGroupId) ->
	openstack_server:delete(NovaPid, ["os-security-groups/", SecurityGroupId]).

%% @doc Deletes a security group rule.
%%
%% @spec delete_rule(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete_rule(NovaPid, SecurityGroupRuleId) when is_integer(SecurityGroupRuleId) ->
	delete_rule(NovaPid, integer_to_list(SecurityGroupRuleId));
delete_rule(NovaPid, SecurityGroupRuleId) ->
	openstack_server:delete(NovaPid, ["os-security-group-rules/", SecurityGroupRuleId]).

%% @doc Gets details of a security group.
%%
%% @spec get(pid(), id()) -> {ok, jsondoc()}    
%% @end
get(NovaPid, SecurityGroupId) when is_integer(SecurityGroupId) ->
	get(NovaPid, integer_to_list(SecurityGroupId));
get(NovaPid, SecurityGroupId) ->
	openstack_server:get(NovaPid, ["os-security-groups/", SecurityGroupId]).

%% @doc Lists all security groups.
%%
%% @spec list(pid()) -> {ok, jsondoc()}    
%% @end
list(NovaPid) ->
	openstack_server:get(NovaPid, "os-security-groups").

%% @doc Lists security groups for a specified server.
%%
%% @spec list_groups_for_server(pid(), id()) -> {ok, jsondoc()}    
%% @end
list_groups_for_server(NovaPid, ServerId) when is_integer(ServerId) ->
	list_groups_for_server(NovaPid, integer_to_list(ServerId));
list_groups_for_server(NovaPid, ServerId) ->
	openstack_server:get(NovaPid, ["servers/", ServerId, "/os-security-groups"]).

%% @doc Removes a group from a server's list of security groups.
%%
%% @spec remove_group_from_server(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
remove_group_from_server(NovaPid, ServerId, GroupName) when is_list(GroupName) ->
	remove_group_from_server(NovaPid, ServerId, list_to_binary(GroupName));
remove_group_from_server(NovaPid, ServerId, GroupName) ->
	Action = #removeSecurityGroup{name=GroupName},
	os_nova:action(NovaPid, ServerId, Action).
