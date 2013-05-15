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

-module(os_glance).

%% Includes
-include("os_glance.hrl").

-export([close/1,
		 create/3,
		 create/5,
		 delete/2,
		 download/3,
		 get/2,
		 list/1,
		 list/2,
		 list_detail/1,
		 list_detail/2,
		 list_shared/2,
		 start/1,
		 start/2,
		 upload/3,
		 upload/5]).

%% API
%% @doc Terminates the process.
%%
%% @spec close(pid()) -> ok    
%% @end
close(Pid) ->
	openstack_server:close(Pid).

%% @doc Adds an image to the Glance repository where the image is passed
%%      as a binary value. It is frequently more convenient to use the
%%      <code>upload/5</code> function instead.
%%
%% @spec create(pid(), string(), binary()) -> {ok, jsondoc()}    
%% @end
create(Pid, Name, Image) ->
	create(Pid, Name, undefined, undefined, Image).

%% @doc Adds an image to the Glance repository where the image is passed
%%      as a binary value. It is frequently more convenient to use the
%%      <code>upload/5</code> function instead.
%%
%% @spec create(pid(), string(), string(), string(), binary()) -> {ok, jsondoc()}    
%% @end
create(Pid, Name, DiskFormat, ContainerFormat, Image) ->
	ImageData = #ximage{container_format=ContainerFormat, disk_format=DiskFormat, name=Name},
	Headers = openstack_records:map_headers("x-image-meta-", ImageData),
	openstack_server:post(Pid, "images", Headers, Image).

%% @doc Deletes an image.
%%
%% @spec delete(pid(), id()) -> ok    
%% @end
delete(Pid, ImageId) when is_integer(ImageId) ->
	delete(Pid, integer_to_list(ImageId));
delete(Pid, ImageId) ->
	openstack_server:delete(Pid, ["images/", ImageId]).
	
%% @doc Downloads an image and writes it to a file.
%%
%% @spec download(pid(), id(), string()) -> {ok, term()}    
%% @end
download(Pid, ImageId, FileName) when is_integer(ImageId) ->
	download(Pid, integer_to_list(ImageId), FileName);
download(Pid, ImageId, FileName) ->
	openstack_server:download(Pid, ["images/", ImageId], FileName).
	
%% @doc Gets the details of an image.
%%
%% @spec get(pid(), id()) -> {ok, jsondoc()}    
%% @end
get(Pid, ImageId) when is_integer(ImageId) ->
	get(Pid, integer_to_list(ImageId));
get(Pid, ImageId) ->
	openstack_server:get_binary(Pid, ["images/", ImageId]).
	
%% @doc Lists all images.
%%
%% @spec list(pid()) -> {ok, jsondoc()}    
%% @end
list(Pid) ->
	openstack_server:get(Pid, "images").

%% @doc Lists all images satisfying some search criteria.
%%
%% @spec list(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list(Pid, Filter) ->
	openstack_server:get(Pid, ["images?", openstack_records:map_filter(Filter)]).
	
%% @doc Lists all images in detail.
%%
%% @spec list_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_detail(Pid) ->
	openstack_server:get(Pid, "images/detail").

%% @doc Lists all images in detail that satisfy some search criteria.
%%
%% @spec list_detail(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_detail(Pid, Filter) ->
	openstack_server:get(Pid, ["images/detail?", openstack_records:map_filter(Filter)]).
	
%% @doc Lists all images shared with some tenant.
%%
%% @spec list_shared(pid(), id()) -> {ok, jsondoc()}    
%% @end
list_shared(Pid, TenantId) when is_integer(TenantId) ->
	list_shared(Pid, integer_to_list(TenantId));
list_shared(Pid, TenantId) ->
	openstack_server:get(Pid, ["shared-images/", TenantId]).
	
%% @doc Spawns a process for interacting with the Glance service given
%%      a process for interacting with Keystone.
%%
%% @spec start(pid()) -> {ok, pid()}    
%% @end
start(KeyStonePid) ->
	{ok, BaseUrl} = os_keystone:get_public_url(KeyStonePid, "image"),
	start(KeyStonePid, BaseUrl).

%% @doc Spawns a process for interacting with the Glance service at a specific
%%      endpoint.
%%
%% @spec start(pid(), url()) -> {ok, pid()}    
%% @end
start(KeyStonePid, BaseUrl) when is_list(BaseUrl) ->
	openstack_server:start(KeyStonePid, BaseUrl);
start(KeyStonePid, BaseUrl) when is_binary(BaseUrl) ->
	openstack_server:start(KeyStonePid, binary_to_list(BaseUrl)).

%% @doc Uploads an image to the Glance repository from a file.
%%
%% @spec upload(pid(), string(), string()) -> {ok, jsondoc()}    
%% @end
upload(Pid, Name,FileName) ->
	upload(Pid, Name, undefined, undefined, FileName).

%% @doc Uploads an image to the Glance repository from a file.
%%
%% @spec upload(pid(), string(), string(), string(), string()) -> {ok, jsondoc()}    
%% @end
upload(Pid, Name, DiskFormat, ContainerFormat, FileName) ->
	ImageData = #ximage{container_format=ContainerFormat, disk_format=DiskFormat, name=Name},
	Headers = openstack_records:map_headers("x-image-meta-", ImageData),
	openstack_server:upload_post(Pid, "images", Headers, FileName).
