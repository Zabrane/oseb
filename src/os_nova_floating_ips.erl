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
%% @doc Functions for working with floating IP addresses using the floating
%%      IPs extension to the Nova API.
%%      

-module(os_nova_floating_ips).

%% Includes
-include("os_nova.hrl").

-export([attach/3,
		 create/1,
		 delete/2,
		 detach/3,
		 list/1]).

%% API
%% @doc Associates a floating IP address with a server.
%%
%% @spec attach(pid(), string(), string()) -> {ok, jsondoc()}    
%% @end
attach(Pid, ServerId, FloatingIp) when is_list(FloatingIp) ->
	attach(Pid, ServerId, list_to_binary(FloatingIp));
attach(Pid, ServerId, FloatingIp) when is_binary(FloatingIp) ->
	Action = #addFloatingIp{address=FloatingIp},
	os_nova:action(Pid, ServerId, Action).	

%% @doc Assigns a floating IP address to the user.
%%
%% @spec create(pid()) -> {ok, jsondoc()}    
%% @end
create(NovaPid) ->
	openstack_server:post(NovaPid, "os-floating-ips").

%% @doc Removes a floating IP address from a server.
%%
%% @spec delete(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete(NovaPid, FloatingIpId) when is_integer(FloatingIpId) ->
	delete(NovaPid, integer_to_list(FloatingIpId));
delete(NovaPid, FloatingIpId) ->
	openstack_server:delete(NovaPid, ["os-floating-ips/", FloatingIpId]).

%% @doc Removes a floating IP address from a server.
%%
%% @spec detach(pid(), string(), string()) -> {ok, jsondoc()}    
%% @end
detach(NovaPid, ServerId, FloatingIp) when is_integer(ServerId) ->
	detach(NovaPid, integer_to_list(ServerId), FloatingIp);
detach(NovaPid, ServerId, FloatingIp) when is_list(FloatingIp) ->
	detach(NovaPid, ServerId, list_to_binary(FloatingIp));
detach(NovaPid, ServerId, FloatingIp) when is_binary(FloatingIp) ->
	Action = #removeFloatingIp{address=FloatingIp},
	os_nova:action(NovaPid, ServerId, Action).	
	
%% @doc Lists all floating IP addresses.
%%
%% @spec list(pid()) -> {ok, jsondoc()}    
%% @end
list(NovaPid) ->
	openstack_server:get(NovaPid, "os-floating-ips").
