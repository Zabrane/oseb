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
%% @doc Functions for interacting with the Glance version 1.1 image service.
%%      

-module(os_swift).

%% Includes
-include("os_swift.hrl").

-export([close/1,
		 copy_object/5,
		 create_container/2,
		 create_container/3,
		 create_object/4,
		 create_object/5,
		 delete_container/2,
		 delete_object/3,
		 download_object/4,
		 get_account_metadata/1,
		 get_container_metadata/2,
		 get_object/3,
		 get_object_metadata/3,
		 list_containers/1,
		 list_containers/2,
		 list_objects/2,
		 list_objects/3,
		 set_account_metadata/2,
		 set_container_metadata/3,
		 set_object_metadata/4,
		 start/1,
		 start/2,
		 upload_object/4,
		 upload_object/5]).

%% API
%% @doc Terminates the process.
%%
%% @spec close(pid()) -> ok    
%% @end
close(Pid) ->
	openstack_server:close(Pid).

%% @doc Copies an object from a source to a destination.
%%
%% @spec copy_object(pid(), string(), string(), string(), string()) -> ok    
%% @end
copy_object(Pid, SrcContainer, SrcObject, DestContainer, DestObject) ->
	XCopyFromHeader = {"X-Copy-From", "/" ++ SrcContainer ++ "/" ++ SrcObject},
	openstack_server:put(Pid,  [DestContainer, "/", DestObject], [XCopyFromHeader], <<>>).

%% @doc Creates a container.
%%
%% @spec create_container(pid(), string()) -> ok    
%% @end
create_container(Pid, Container) ->
	create_container(Pid, Container, []).

%% @doc Creates a container with specified metadata.
%%
%% @spec create_container(pid(), string(), proplist()) -> ok    
%% @end
create_container(Pid, Container, Metadata) ->
	openstack_server:put(Pid, Container, Metadata, <<>>).

%% @doc Creates an object.
%%
%% @spec create_object(pid(), string(), string(), binary()) -> ok    
%% @end
create_object(Pid, Container, ObjectName, ObjectValue) ->
	create_object(Pid, Container, ObjectName, [], ObjectValue).

%% @doc Creates an object with specified metadata.
%%
%% @spec create_object(pid(), string(), string(), proplist(), binary()) -> ok    
%% @end
create_object(Pid, Container, ObjectName, ObjectMetadata, ObjectValue) ->
	openstack_server:put(Pid, [Container, "/", ObjectName], ObjectMetadata, ObjectValue).

%% @doc Deletes a container.
%%
%% @spec delete_container(pid(), string()) -> ok    
%% @end
delete_container(Pid, Container) ->
	openstack_server:delete(Pid, Container).

%% @doc Deletes an object from a container.
%%
%% @spec delete_object(pid(), string(), string()) -> ok    
%% @end
delete_object(Pid, Container, Object) ->
	openstack_server:delete(Pid, [Container, "/", Object]).

%% @doc Retrieves an object and saves it to a file.
%%
%% @spec download_object(pid(), string(), string(), string()) -> {ok, saved_to_file}    
%% @end
download_object(Pid, Container, Object, FileName) ->
	openstack_server:download(Pid, [Container, "/", Object], FileName).

%% @doc Gets account metadata.
%%
%% @spec get_account_metadata(pid()) -> {ok, list()}    
%% @end
get_account_metadata(Pid) ->
	case openstack_server:head(Pid, "") of
		{ok, Headers} ->
			openstack_records:filter_headers("x-account-", Headers);
		Error ->
			Error
	end.

%% @doc Gets container metadata.
%%
%% @spec get_container_metadata(pid(), string()) -> {ok, list()}    
%% @end
get_container_metadata(Pid, Container) ->
	case openstack_server:head(Pid, Container) of
		{ok, Headers} ->
			openstack_records:filter_headers("x-container-", Headers);
		Error ->
			Error
	end.

%% @doc Retrieves an object.
%%
%% @spec get_object(pid(), string(), string()) -> {ok, binary()}    
%% @end
get_object(Pid, Container, Object) ->
	openstack_server:get_binary(Pid, [Container, "/", Object]).

%% @doc Gets an object's metadata.
%%
%% @spec get_object_metadata(pid(), string(), string()) -> {ok, list()}    
%% @end
get_object_metadata(Pid, Container, Object) ->
	openstack_server:head(Pid, [Container, "/", Object]).

%% @doc Lists the containers.
%%
%% @spec list_containers(pid()) -> {ok, jsondoc()}    
%% @end
list_containers(Pid) ->
	list_containers(Pid, #container_filter{}).
	
%% @doc Lists the containers with the specified query parameters.
%%
%% @spec list_containers(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_containers(Pid, Filter) ->
	openstack_server:get(Pid, ["?", openstack_records:map_filter(Filter)]).
	
%% @doc Lists the objects in a container.
%%
%% @spec list_objects(pid(), string()) -> {ok, jsondoc()}    
%% @end
list_objects(Pid, Container) ->
	list_objects(Pid, Container, #object_filter{}).
	
%% @doc Lists the objects in a container with the specified query parameters.
%%
%% @spec list_objects(pid(), string(), filter()) -> {ok, jsondoc()}    
%% @end
list_objects(Pid, Container, Filter) ->
	openstack_server:get(Pid, [Container, "?", openstack_records:map_filter(Filter)]).
	
%% @doc Sets account metadata.
%%
%% @spec set_account_metadata(pid(), proplist()) -> ok    
%% @end
set_account_metadata(Pid, AccountMetadata) ->
	openstack_server:post(Pid, "", AccountMetadata, <<>>).

%% @doc Sets container metadata.
%%
%% @spec set_container_metadata(pid(), string(), proplist()) -> ok    
%% @end
set_container_metadata(Pid, Container, ContainerMetadata) ->
	openstack_server:post(Pid, Container, ContainerMetadata, <<>>).

%% @doc Sets object metadata.
%%
%% @spec set_object_metadata(pid(), string(), string(), proplist()) -> ok    
%% @end
set_object_metadata(Pid, Container, Object, ContainerMetadata) ->
	openstack_server:post(Pid, [Container, "/", Object], ContainerMetadata, <<>>).

%% @doc Spawns a process for interacting with the Swift service given
%%      a process for interacting with Keystone.
%%
%% @spec start(pid()) -> {ok, pid()}    
%% @end
start(KeyStonePid) ->
	{ok, BaseUrl} = os_keystone:get_public_url(KeyStonePid, "object-store"),
	start(KeyStonePid, BaseUrl).

%% @doc Spawns a process for interacting with the Swift service at a specific
%%      endpoint.
%%
%% @spec start(pid(), url()) -> {ok, pid()}    
%% @end
start(KeyStonePid, BaseUrl) when is_list(BaseUrl) ->
	openstack_server:start(KeyStonePid, BaseUrl);
start(KeyStonePid, BaseUrl) when is_binary(BaseUrl) ->
	openstack_server:start(KeyStonePid, binary_to_list(BaseUrl)).

%% @doc Uploads an object.
%%
%% @spec upload_object(pid(), string(), string(), string()) -> ok    
%% @end
upload_object(Pid, Container, ObjectName, FileName) ->
	upload_object(Pid, Container, ObjectName, [], FileName).

%% @doc Uploads an object with specified metadata.
%%
%% @spec upload_object(pid(), string(), string(), proplist(), binary()) -> ok    
%% @end
upload_object(Pid, Container, ObjectName, ObjectMetadata, FileName) ->
	openstack_server:upload_put(Pid, [Container, "/", ObjectName], ObjectMetadata, FileName).
