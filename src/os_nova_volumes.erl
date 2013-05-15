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
%% @doc Functions for interacting with the volumes extensions to Nova.
%%      This module also has functions for interacting with volume attachments
%%      and volume types although that functionality is not part of the extension.
%%      

-module(os_nova_volumes).

%% Includes
-include("os_nova.hrl").

-export([attach/3,
		 create/2,
		 create_snapshot/2,
		 detach/3,
		 delete/2,
		 delete_snapshot/2,
		 get/2,
		 get_attachment/3,
		 get_snapshot/2,
		 get_type/2,
		 list/1,
		 list/2,
		 list_attachments/2,
		 list_detail/1,
		 list_detail/2,
		 list_snapshots/1,
		 list_snapshots_detail/1,
		 list_types/1]).

%% API Functions
%% @doc Attaches a volume to a server.
%%
%% @spec attach(pid(), id(), jsondoc()) -> {ok, jsondoc()}    
%% @end
attach(NovaPid, ServerId, VolumeAttachmentParameters) when is_integer(ServerId) ->
	attach(NovaPid, integer_to_list(ServerId), VolumeAttachmentParameters);
attach(NovaPid, ServerId, VolumeAttachmentParameters) ->
	openstack_server:post(NovaPid, ["servers/", ServerId, "/os-volume_attachments"] , VolumeAttachmentParameters).
	
%% @doc Creates a volume.
%%
%% @spec create(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create(NovaPid, VolumeParameters) ->
	openstack_server:post(NovaPid, "os-volumes", VolumeParameters).

%% @doc Creates a snapshot of a volume.
%%
%% @spec create_snapshot(pid(), jsondoc()) -> {ok, jsondoc()}    
%% @end
create_snapshot(NovaPid, SnapshotParameters) ->
	openstack_server:post(NovaPid, "os-snapshots", SnapshotParameters).
	
%% @doc Deletes a volume.
%%
%% @spec delete(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete(NovaPid, VolumeId) when is_integer(VolumeId) ->
	delete(NovaPid, integer_to_list(VolumeId));
delete(NovaPid, VolumeId) ->
	openstack_server:delete(NovaPid, ["os-volumes/", VolumeId]).

%% @doc Deletes a volume snapshot.
%%
%% @spec delete_snapshot(pid(), id()) -> {ok, jsondoc()}    
%% @end
delete_snapshot(NovaPid, SnapshotId) when is_integer(SnapshotId) ->
	delete_snapshot(NovaPid, integer_to_list(SnapshotId));
delete_snapshot(NovaPid, SnapshotId) ->
	openstack_server:delete(NovaPid, ["os-snapshots/", SnapshotId]).

%% @doc Detaches a volume from a server.
%%
%% @spec detach(pid(), id(), id()) -> ok    
%% @end
detach(NovaPid, ServerId, VolumeId) when is_integer(ServerId) ->
	detach(NovaPid, integer_to_list(ServerId), VolumeId);
detach(NovaPid, ServerId, VolumeId) when is_integer(VolumeId) ->
	detach(NovaPid, ServerId, integer_to_list(VolumeId));
detach(NovaPid, ServerId, VolumeId) ->
	openstack_server:delete(NovaPid, ["servers/", ServerId, "/os-volume_attachments/", VolumeId]).
	
%% @doc Gets the details of a volume.
%%
%% @spec get(pid(), id()) -> {ok, jsondoc()}    
%% @end
get(NovaPid, VolumeId) when is_integer(VolumeId) ->
	get(NovaPid, integer_to_list(VolumeId));
get(NovaPid, VolumeId) ->
	openstack_server:get(NovaPid, ["os-volumes/", VolumeId]).

%% @doc Gets the details of a volume's attachment to a server.
%%
%% @spec get_attachment(pid(), id(), id()) -> {ok, jsondoc()}    
%% @end
get_attachment(NovaPid, ServerId, AttachmentId) when is_integer(ServerId) ->
	get_attachment(NovaPid, integer_to_list(ServerId), AttachmentId);
get_attachment(NovaPid, ServerId, AttachmentId) when is_integer(AttachmentId) ->
	get_attachment(NovaPid, ServerId, integer_to_list(AttachmentId));
get_attachment(NovaPid, ServerId, AttachmentId) ->
	openstack_server:get(NovaPid, ["servers/", ServerId, "/os-volume_attachments/", AttachmentId]).

%% @doc Gets the details of a volume snapshot.
%%
%% @spec get_snapshot(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_snapshot(NovaPid, SnapshotId) when is_integer(SnapshotId) ->
	get_snapshot(NovaPid, integer_to_list(SnapshotId));
get_snapshot(NovaPid, SnapshotId) ->
	openstack_server:get(NovaPid, ["os-snapshots/", SnapshotId]).

%% @doc Gets the details of a volume type.
%%
%% @spec get_type(pid(), id()) -> {ok, jsondoc()}    
%% @end
get_type(NovaPid, TypeId) when is_integer(TypeId) ->
	get_type(NovaPid, integer_to_list(TypeId));
get_type(NovaPid, TypeId) ->
	openstack_server:get(NovaPid, ["os-volume-types/", TypeId]).

%% @doc Lists all volumes.
%%
%% @spec list(pid()) -> {ok, jsondoc()}    
%% @end
list(NovaPid) ->
	openstack_server:get(NovaPid, "os-volumes").

%% @doc Lists all volumes satisfying some search or limit criteria.
%%
%% @spec list(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list(NovaPid, Filter) ->
	openstack_server:get(NovaPid, ["os-volumes?", openstack_records:map_filter(Filter)]).

%% @doc Lists all volumes attachments.
%%
%% @spec list_attachments(pid(), id()) -> {ok, jsondoc()}    
%% @end
list_attachments(NovaPid, ServerId) when is_integer(ServerId) ->
	list_attachments(NovaPid, integer_to_list(ServerId));
list_attachments(NovaPid, ServerId) ->
	openstack_server:get(NovaPid, ["servers/", ServerId, "/os-volume_attachments"]).

%% @doc Lists all volumes in detail.
%%
%% @spec list_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_detail(NovaPid) ->
	openstack_server:get(NovaPid, "os-volumes").

%% @doc Lists all volumes satisfying some search or limit criteria in detail.
%%
%% @spec list_detail(pid(), filter()) -> {ok, jsondoc()}    
%% @end
list_detail(NovaPid, Filter) ->
	openstack_server:get(NovaPid, ["os-volumes/detail?", openstack_records:map_filter(Filter)]).

%% @doc Lists all volume snapshots.
%%
%% @spec list_snapshots(pid()) -> {ok, jsondoc()}    
%% @end
list_snapshots(NovaPid) ->
	openstack_server:get(NovaPid, "os-snapshots").

%% @doc Lists all volume snapshots in detail.
%%
%% @spec list_snapshots_detail(pid()) -> {ok, jsondoc()}    
%% @end
list_snapshots_detail(NovaPid) ->
	openstack_server:get(NovaPid, "os-snapshots/detail").

%% @doc Lists all volume types.
%%
%% @spec list_types(pid()) -> {ok, jsondoc()}    
%% @end
list_types(NovaPid) ->
	openstack_server:get(NovaPid, "os-volume-types").
