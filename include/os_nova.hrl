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

-include("os_cinder.hrl").

%% Nova compute
-record(server, {tenant_id, security_group, security_groups, user_data, availability_zone, name, flavorRef, imageRef, metadata, personality, key_name}).
-record(security_group, {name, description, rules, tenant_id, id}).
-record(security_group_rule, {ip_protocol, from_port, to_port, cidr, group_id, parent_group_id}).
-record(personality, {path, contents}).

%% Nova volumes
-record(snapshot, {volume_id, force, display_name, display_description}).

%% Nova keypairs
-record(keypair, {name, public_key}).


%% Server actions
-record(changePassword, {adminPass}).
-record(reboot, {type}).
-record(rebuild, {imageRef, name, adminPass, accessIPv4, accessIPv6, metadata, personality}).
-record(createImage, {name, metadata}).
-record(resize, {flavorRef}).
-record(addSecurityGroup, {name}).
-record(removeSecurityGroup, {name}).

%% Nova floating IP actions
-record(addFloatingIp, {address}).
-record(removeFloatingIp, {address}).


%% Filters
%% Nova
-record(filter_server, {'changes-since', image, flavor, name, marker, limit,status}).
-record(filter_nova_image, {'changes-since', server, name, status, marker, limit, type}).
-record(filter_nova_volume, {'changes-since', name, status, size}).
