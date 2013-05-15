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
%% @doc Functions for interacting with the Cinder block storage service.
%% @end

-module(os_cinder).

%% Includes
-include("os_cinder.hrl").

%% External functions
-export([attach/3,
		 close/1,
		 create/2,
		 create_snapshot/2,
		 delete/2,
		 delete_snapshot/2,
		 detach/3,
		 get/2,
		 get_attachment/3,
		 get_snapshot/2,
		 get_type/2,
		 list/1,
		 list_attachments/2,
		 list_detail/1,
		 list_snapshots/1,
		 list_snapshots_detail/1,
		 list_types/1,
		 start/1,
		 start/2]).


%% API Functions
%% @doc Attaches a volume to a server.
%%
%% @spec attach(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
attach(Pid, ServerId, VolumeAttachmentParameters) when is_integer(ServerId) ->
	attach(Pid, integer_to_list(ServerId), VolumeAttachmentParameters);
attach(Pid, ServerId, VolumeParameters) ->
	openstack_server:post(Pid, ["servers/", ServerId, "/os-volume_attachments"] , VolumeParameters).
	
%% @doc Terminates the process.
%%
%% @spec close(pid()) -> ok    
%% @end
close(Pid) ->
	openstack_server:close(Pid).

%% @doc Creates a volume.
%%
%% @spec create(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create(Pid, VolumeParameters) ->
	openstack_server:post(Pid, "volumes", VolumeParameters).

%% @doc Creates a snapshot of a volume.
%%
%% @spec create_snapshot(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create_snapshot(Pid, SnapshotParameters) ->
	openstack_server:post(Pid, "snapshots", SnapshotParameters).
	
%% @doc Deletes a volume.
%%
%% @spec delete(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete(Pid, VolumeId) when is_integer(VolumeId) ->
	delete(Pid, integer_to_list(VolumeId));
delete(Pid, VolumeId) ->
	openstack_server:delete(Pid, ["volumes/", VolumeId]).

%% @doc Deletes a volume snapshot.
%%
%% @spec delete_snapshot(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete_snapshot(Pid, SnapshotId) when is_integer(SnapshotId) ->
	delete_snapshot(Pid, integer_to_list(SnapshotId));
delete_snapshot(Pid, SnapshotId) ->
	openstack_server:delete(Pid, ["snapshots/", SnapshotId]).

%% @doc Detaches a volume from a server.
%%
%% @spec detach(pid(), id(), id()) -> ok    
%% @end
detach(Pid, ServerId, VolumeId) when is_integer(ServerId) ->
	detach(Pid, integer_to_list(ServerId), VolumeId);
detach(Pid, ServerId, VolumeId) when is_integer(VolumeId) ->
	detach(Pid, ServerId, integer_to_list(VolumeId));
detach(Pid, ServerId, VolumeId) ->
	openstack_server:delete(Pid, ["servers/", ServerId, "/os-volume_attachments/", VolumeId]).
	
%% @doc Gets the details of a volume.
%%
%% @spec get(pid(), id()) -> {ok, jsondoc()}    
%% @end
get(Pid, VolumeId) when is_integer(VolumeId) ->
	get(Pid, integer_to_list(VolumeId));
get(Pid, VolumeId) ->
	openstack_server:get(Pid, ["volumes/", VolumeId]).

%% @doc Gets the details of a volume's attachment to a server.
%%
%% @spec get_attachment(pid(), id(), id()) -> {ok, jsondoc()}    
%% @end
get_attachment(Pid, ServerId, AttachmentId) when is_integer(ServerId) ->
	get_attachment(Pid, integer_to_list(ServerId), AttachmentId);
get_attachment(Pid, ServerId, AttachmentId) when is_integer(AttachmentId) ->
	get_attachment(Pid, ServerId, integer_to_list(AttachmentId));
get_attachment(Pid, ServerId, AttachmentId) ->
	openstack_server:get(Pid, ["servers/", ServerId, "/os-volume_attachments/", AttachmentId]).

%% @doc Gets the details of a volume snapshot.
%%
%% @spec get_snapshot(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_snapshot(Pid, SnapshotId) when is_integer(SnapshotId) ->
	get_snapshot(Pid, integer_to_list(SnapshotId));
get_snapshot(Pid, SnapshotId) ->
	openstack_server:get(Pid, ["snapshots/", SnapshotId]).

%% @doc Gets the details of a volume type.
%%
%% @spec get_type(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_type(Pid, TypeId) when is_integer(TypeId) ->
	get_type(Pid, integer_to_list(TypeId));
get_type(Pid, TypeId) ->
	openstack_server:get(Pid, ["types/", TypeId]).

%% @doc Lists all volumes.
%%
%% @spec list(pid()) -> {ok, jsondoc()}    
%% @end
list(Pid) ->
	openstack_server:get(Pid, "volumes").

%% @doc Lists all volumes attachments.
%%
%% @spec list_attachments(pid(), id()) -> {ok, jsondoc()}    
%% @end
list_attachments(Pid, ServerId) when is_integer(ServerId) ->
	list_attachments(Pid, integer_to_list(ServerId));
list_attachments(Pid, ServerId) ->
	openstack_server:get(Pid, ["servers/", ServerId, "/os-volume_attachments"]).

%% @doc Lists all volumes in detail.
%%
%% @spec list_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_detail(Pid) ->
	openstack_server:get(Pid, "volumes/detail").

%% @doc Lists all volume snapshots.
%%
%% @spec list_snapshots(pid()) -> {ok, jsondoc()}    
%% @end
list_snapshots(Pid) ->
	openstack_server:get(Pid, "snapshots").

%% @doc Lists all volume snapshots in detail.
%%
%% @spec list_snapshots_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_snapshots_detail(Pid) ->
	openstack_server:get(Pid, "snapshots/detail").

%% @doc Lists all volume types.
%%
%% @spec list_types(pid()) -> {ok, jsondoc()}    
%% @end
list_types(Pid) ->
	openstack_server:get(Pid, "types").

%% @doc Spawns a process for interacting with the Cinder service given
%%      a process for interacting with Keystone.
%%
%% @spec start(pid()) -> {ok, pid()}    
%% @end
start(KeystonePid) ->
	{ok, BaseUrl} = os_keystone:get_public_url(KeystonePid, "volume"),
	start(KeystonePid, BaseUrl).

%% @doc Spawns a process for interacting with the Cinder service at a specific
%%      endpoint.
%%
%% @spec start(pid(), url()) -> {ok, pid()}    
%% @end
start(KeyStonePid, BaseUrl) when is_list(BaseUrl) ->
	openstack_server:start(KeyStonePid, BaseUrl);
start(KeyStonePid, BaseUrl) when is_binary(BaseUrl) ->
	openstack_server:start(KeyStonePid, binary_to_list(BaseUrl)).

