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
%% @doc Functions for interacting with the Nova compute service.<br/><br/>
%%      This module exposes only the core functionality of OpenStack Nova
%%      for interacting with servers, flavors and images. 
%%      Other modules expose the functionality of extensions to Nova.
%% @end

-module(os_nova).

%% Includes
-include("os_nova.hrl").

-export([action/3,
		 change_password/3,
		 close/1,
		 confirm_resize/2,
		 create_image/3,
		 create_server/2,
		 delete_image/2,
		 delete_metadata_item/3,
		 delete_server/2,
		 get_extension_details/2,
		 get_flavor/2,
		 get_image_details/2,
		 get_metadata_item/3,
		 get_server/2,
		 list_addresses/2,
		 list_addresses_by_network/3,
		 list_extensions/1,
		 list_flavors/1,
		 list_flavors_detail/1,
		 list_images/1,
		 list_images/2,
		 list_images_detail/1,
		 list_images_detail/2,
		 list_limits/1,
		 list_metadata/2,
		 list_servers/1,
		 list_servers/2,
		 list_servers_detail/1,
		 list_servers_detail/2,
		 reboot/2,
		 reboot/3,
		 rebuild/3,
		 resize/3,
		 revert_resize/2,
		 set_metadata/3,
		 set_metadata_item/4,
		 start/1,
		 start/2,
		 update_server/3,
		 update_metadata/3]).

%% API
%% @doc Performs an action on a server such as rebooting or changing
%%      the root password.
%%
%% @spec action(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
action(Pid, ServerId, Action) when is_integer(ServerId) ->
	action(Pid, integer_to_list(ServerId), Action);
action(Pid, ServerId, Action) ->
	openstack_server:post(Pid, ["servers/", ServerId, "/action" ], Action).

%% @doc Changes the root password for a server.
%%
%% @spec change_password(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
change_password(Pid, ServerId, Password) when is_list(Password) ->
	change_password(Pid, ServerId, list_to_binary(Password));
change_password(Pid, ServerId, Password) ->
	Action = #changePassword{adminPass= Password},
	action(Pid, ServerId, Action).

%% @doc Terminates the process.
%%
%% @spec close(pid()) -> ok    
%% @end
close(Pid) ->
	openstack_server:close(Pid).

%% @doc Confirms the resizing of a server.
%%
%% @spec confirm_resize(pid(), id()) -> {ok, jsondoc()}    
%% @end
confirm_resize(Pid, ServerId) ->
	Action = {[{<<"confirmResize">>, null}]},
	action(Pid, ServerId, Action).
	
%% @doc Creates an image of a server with a specified name.
%%
%% @spec create_image(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
create_image(Pid, ServerId, Name) when is_integer(ServerId) ->
	create_image(Pid, integer_to_list(ServerId), Name);
create_image(Pid, ServerId, Name) when is_list(Name) ->
	create_image(Pid, ServerId, list_to_binary(Name));
create_image(Pid, ServerId, Name) ->
	Action = #createImage{name=Name},
	action(Pid, ServerId, Action).

%% @doc Creates a new server instance using specified server parameters.
%%      The server parameters include an image reference, a flavor reference,
%%      a name, etc.
%%
%% @spec create_server(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create_server(Pid, ServerParameters) ->
	openstack_server:post(Pid, "servers", ServerParameters).
	
%% @doc Terminates a server.
%%
%% @spec delete_server(pid(), id()) -> ok    
%% @end
delete_server(Pid, ServerId) when is_integer(ServerId) ->
	delete_server(Pid, integer_to_list(ServerId));
delete_server(Pid, ServerId) ->
	openstack_server:delete(Pid, ["servers/", ServerId]).

%% @doc Deletes a virtual machine image.
%%
%% @spec delete_image(pid(), id()) -> ok    
%% @end
delete_image(Pid, ImageId) when is_integer(ImageId) ->
	delete_image(Pid, integer_to_list(ImageId));
delete_image(Pid, ImageId) ->
	openstack_server:delete(Pid, ["images/", ImageId]).

%% @doc Deletes an item of metadata associated with a server
%%
%% @spec delete_metadata_item(pid(), id(), string()) -> ok    
%% @end
delete_metadata_item(Pid, ServerId, Key) when is_integer(ServerId) ->
	delete_metadata_item(Pid, integer_to_list(ServerId), Key);
delete_metadata_item(Pid, ServerId, Key) ->
	openstack_server:delete(Pid, ["servers/", ServerId, "/metadata/", Key]).

%% @doc Gets the details about an extension to the Nova API.
%%
%% @spec get_extension_details(pid(), string()) -> {ok, jsondoc()}    
%% @end
get_extension_details(Pid, Alias) ->
	openstack_server:get(Pid, ["extensions/", Alias]).

%% @doc Gets the specification of a flavor.
%%
%% @spec get_flavor(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_flavor(Pid, FlavorId) when is_integer(FlavorId) ->
	get_flavor(Pid, integer_to_list(FlavorId));
get_flavor(Pid, FlavorId) ->
	openstack_server:get(Pid, ["flavors/", FlavorId]).

%% @doc Gets details of a virtual machine image.
%%
%% @spec get_image_details(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_image_details(Pid, ImageId) when is_integer(ImageId) ->
	get_image_details(Pid, integer_to_list(ImageId));
get_image_details(Pid, ImageId) ->
	openstack_server:get(Pid, ["images/", ImageId]).

%% @doc Gets the value of an item of metadata associated with a server.
%%
%% @spec get_metadata_item(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
get_metadata_item(Pid, ServerId, Key) when is_integer(ServerId) ->
	get_metadata_item(Pid, integer_to_list(ServerId), Key);
get_metadata_item(Pid, ServerId, Key) ->
	openstack_server:get(Pid, ["servers/", ServerId, "/metadata/", Key]).

%% @doc Gets details of a server.
%%
%% @spec get_server(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_server(Pid, ServerId) when is_integer(ServerId) ->
	get_server(Pid, integer_to_list(ServerId));
get_server(Pid, ServerId) ->
	openstack_server:get(Pid, ["servers/", ServerId]).

%% @doc Lists the IP addresses associated with a server.
%%
%% @spec list_addresses(pid(), id()) -> {ok, jsondoc()}    
%% @end
list_addresses(Pid, ServerId) when is_integer(ServerId) ->
	list_addresses(Pid, integer_to_list(ServerId));
list_addresses(Pid, ServerId) ->
	openstack_server:get(Pid, ["servers/", ServerId, "/ips"]).

%% @doc Lists the IP addresses associated with a server for a
%%      specific network.
%%
%% @spec list_addresses_by_network(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
list_addresses_by_network(Pid, ServerId, Network) when is_integer(ServerId) ->
	list_addresses_by_network(Pid, integer_to_list(ServerId), Network);
list_addresses_by_network(Pid, ServerId, Network) ->
	openstack_server:get(Pid, ["servers/", ServerId, "/ips/", Network]).

%% @doc Lists the supported extensions to the Nova API.
%%
%% @spec list_extensions(pid()) -> {ok, jsondoc()}    
%% @end
list_extensions(Pid) ->
	openstack_server:get(Pid, "extensions").

%% @doc Lists the flavors of hosts supported by the API.
%%
%% @spec list_flavors(pid()) -> {ok, jsondoc()}    
%% @end
list_flavors(Pid) ->
	openstack_server:get(Pid, "flavors").

%% @doc Lists the flavors of hosts supported by the API
%%      in details.
%%
%% @spec list_flavors_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_flavors_detail(Pid) ->
	openstack_server:get(Pid, "flavors/detail").

%% @doc Lists virtual machine images.
%%
%% @spec list_images(pid()) -> {ok, jsondoc()}    
%% @end
list_images(Pid) ->
	openstack_server:get(Pid, "images").

%% @doc Lists virtual machine images satisfying some filtering and/or
%%      limit criteria.
%%
%% @spec list_images(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_images(Pid, Filter) ->
	openstack_server:get(Pid, ["images?", openstack_records:map_filter(Filter)]).

%% @doc Lists virtual machine images in detail.
%%
%% @spec list_images_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_images_detail(Pid) ->
	openstack_server:get(Pid, "images/detail").

%% @doc Lists virtual machine images satisfying some filtering and/or
%%      limit criteria.
%%
%% @spec list_images_detail(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_images_detail(Pid, Filter) when is_list(Filter) ->
	openstack_server:get(Pid, ["images/detail?", Filter]);
list_images_detail(Pid, Filter) when is_tuple(Filter) ->
	openstack_server:get(Pid, ["images/detail?", openstack_records:map_filter(Filter)]).

%% @doc Lists the limits of an account.
%%
%% @spec list_limits(pid()) -> {ok, jsondoc()}    
%% @end
list_limits(Pid) ->
	openstack_server:get(Pid, "limits").

%% @doc Lists the metadata associated with a server.
%%
%% @spec list_metadata(pid(), id()) -> {ok, jsondoc()}    
%% @end
list_metadata(Pid, ServerId) when is_integer(ServerId) ->
	list_metadata(Pid, integer_to_list(ServerId));
list_metadata(Pid, ServerId) ->
	openstack_server:get(Pid, ["servers/", ServerId, "/metadata"]).

%% @doc Lists the servers.
%%
%% @spec list_servers(pid()) -> {ok, jsondoc()}    
%% @end
list_servers(Pid) ->
	openstack_server:get(Pid, "servers").

%% @doc Lists the servers satisfying some search criteria.
%%
%% @spec list_servers(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_servers(Pid, Filter) ->
	openstack_server:get(Pid, ["servers?", openstack_records:map_filter(Filter)]).

%% @doc Lists the servers in detail.
%%
%% @spec list_servers_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_servers_detail(Pid) ->
	openstack_server:get(Pid, "servers/detail").

%% @doc Lists the servers satisfying some search criteria.
%%
%% @spec list_servers_detail(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_servers_detail(Pid, Filter) when is_list(Filter) ->
	openstack_server:get(Pid, ["servers/detail?", Filter]);
list_servers_detail(Pid, Filter) when is_tuple(Filter) ->
	openstack_server:get(Pid, ["servers/detail?", openstack_records:map_filter(Filter)]).

%% @doc Performs a hard reboot of a server.
%%
%% @spec reboot(pid(), id()) -> {ok, jsondoc()}    
%% @end
reboot(Pid, ServerId) ->
	reboot(Pid, ServerId, <<"HARD">>).

%% @doc Performs a hard or soft reboot of a server.
%%
%% @spec reboot(pid(), id(), string()) -> {ok, jsondoc()}    
%% @end
reboot(Pid, ServerId, RebootType) when is_list(RebootType) ->
	reboot(Pid, ServerId, list_to_binary(RebootType));
reboot(Pid, ServerId, RebootType) ->
	Action = #reboot{type= RebootType},
	action(Pid, ServerId, Action).

%% @doc Rebuilds a server from a specified machine image.
%%      If you want to change other server parameters as part
%%      of the rebuild, use the <code>action/3</code> function
%%      instead.
%%
%% @spec rebuild(pid(), id(), id()) -> {ok, jsondoc()}    
%% @end
rebuild(Pid, ServerId, ImageRef) when is_list(ImageRef) ->
	rebuild(Pid, ServerId, list_to_binary(ImageRef));
rebuild(Pid, ServerId, ImageRef) when is_integer(ImageRef) ->
	rebuild(Pid, ServerId, integer_to_list(ImageRef));
rebuild(Pid, ServerId, ImageRef) ->
	Action = #rebuild{imageRef=ImageRef},
	action(Pid, ServerId, Action).

%% @doc Resizes a server.
%%
%% @spec resize(pid(), id(), id()) -> {ok, jsondoc()}    
%% @end
resize(Pid, ServerId, FlavorRef) when is_list(FlavorRef) ->
	resize(Pid, ServerId, list_to_binary(FlavorRef));
resize(Pid, ServerId, FlavorRef) when is_integer(FlavorRef) ->
	resize(Pid, ServerId, integer_to_list(FlavorRef));
resize(Pid, ServerId, FlavorRef) when is_binary(FlavorRef) ->
	Action = #resize{flavorRef=FlavorRef},
	action(Pid, ServerId, Action).
	
%% @doc Reverts the resizing of a server.
%%
%% @spec revert_resize(pid(), id()) -> {ok, jsondoc()}    
%% @end
revert_resize(Pid, ServerId) ->
	Action = {[{<<"revertResize">>, null}]},
	action(Pid, ServerId, Action).
	
%% @doc Sets the metadata associated with a server.
%%
%% @spec set_metadata(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
set_metadata(Pid, ServerId, JsonMetadata) when is_integer(ServerId) ->
	set_metadata(Pid, integer_to_list(ServerId), JsonMetadata);
set_metadata(Pid, ServerId, JsonMetadata) ->
	openstack_server:put(Pid, ["servers/", ServerId, "/metadata"], JsonMetadata).

%% @doc Sets an item of metadata associated with a server.
%%
%% @spec set_metadata_item(pid(), id(), string(), string()) -> {ok, jsondoc()}    
%% @end
set_metadata_item(Pid, ServerId, Key, Value) when is_integer(ServerId) ->
	set_metadata_item(Pid, integer_to_list(ServerId), Key, Value);
set_metadata_item(Pid, ServerId, Key, Value) when is_binary(Key), is_binary(Value) ->
	JsonKeyValue = {[{Key, Value}]},
	JsonMetadata = {[{<<"meta">>, JsonKeyValue}]},
	openstack_server:put(Pid, ["servers/", ServerId, "/metadata/", Key], JsonMetadata);
set_metadata_item(Pid, ServerId, Key, Value) when is_list(Key) ->
	set_metadata_item(Pid, ServerId, list_to_binary(Key), Value);
set_metadata_item(Pid, ServerId, Key, Value) when is_list(Value) ->
	set_metadata_item(Pid, ServerId, Key, list_to_binary(Value));
set_metadata_item(Pid, ServerId, Key, Value) when is_integer(Value) ->
	set_metadata_item(Pid, ServerId, Key, integer_to_list(Value));
set_metadata_item(Pid, ServerId, Key, Value) when is_float(Value) ->
	set_metadata_item(Pid, ServerId, Key, float_to_list(Value)).

%% @doc Spawns a process for interacting with the Nova service given
%%      a process for interacting with Keystone.
%%
%% @spec start(pid()) -> {ok, pid()}    
%% @end
start(KeystonePid) ->
	{ok, BaseUrl} = os_keystone:get_public_url(KeystonePid, "compute"),
	start(KeystonePid, BaseUrl).

%% @doc Spawns a process for interacting with the Nova service at a specific
%%      endpoint.
%%
%% @spec start(pid(), url()) -> {ok, pid()}    
%% @end
start(KeyStonePid, BaseUrl) when is_list(BaseUrl) ->
	openstack_server:start(KeyStonePid, BaseUrl);
start(KeyStonePid, BaseUrl) when is_binary(BaseUrl) ->
	openstack_server:start(KeyStonePid, binary_to_list(BaseUrl)).

%% @doc Changes the name or password of a server.
%%
%% @spec update_server(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
update_server(Pid, ServerId, ServerParameters) when is_integer(ServerId) ->
	update_server(Pid, integer_to_list(ServerId), ServerParameters);
update_server(Pid, ServerId, ServerParameters) ->
	openstack_server:put(Pid, ["servers/", ServerId], ServerParameters).

%% @doc Updates a server's metadata.
%%
%% @spec update_metadata(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
update_metadata(Pid, ServerId, JsonMetadata) when is_integer(ServerId) ->
	update_metadata(Pid, integer_to_list(ServerId), JsonMetadata);
update_metadata(Pid, ServerId, JsonMetadata) ->
	openstack_server:post(Pid, ["servers/", ServerId, "/metadata"], JsonMetadata).
