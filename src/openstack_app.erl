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
%% @doc An OpenStack application for accessing Keystone, Nova, Glance and Cinder services.<br/><br/> 
%% @end     

-module(openstack_app).

-behaviour(application).

%% Includes
-include("openstack.hrl").
-include("os_glance.hrl").
-include("os_keystone.hrl").
-include("os_nova.hrl").
-include("os_swift.hrl").

%% External exports
-export([start/0]).

%% Behavioural exports
-export([start/2,
		 stop/1]).

%% API
%% @doc A convenience function to simplify starting the openstack app.
%%      You can invoke <code>openstack_app:start()</code> instead of 
%%      explicitly starting this application's dependencies and then
%%      calling <code>application:start(openstack)</code>.
start() ->
	start(openstack).

%% Behavioural functions
%% @private
-spec(start(any(), any()) -> {ok, pid()} | {error, Reason::any()}).
start(_Type, _StartArgs) ->
	TableId = ets:new(openstack_table, [public]),
	Response = openstack_sup:start_link(TableId),
	ok = openstack_records:add_mapping(?mapping(auth)),
	ok = openstack_records:add_mapping(?mapping(passwordCredentials)),
	ok = openstack_records:add_mapping(?mapping(apiAccessKeyCredentials)),
	ok = openstack_records:add_mapping(?mapping(user)),
	ok = openstack_records:add_mapping(?mapping(tenant)),
	ok = openstack_records:add_mapping(?mapping('OS-KSADM:service')),
	ok = openstack_records:add_mapping(?mapping(role)),
	ok = openstack_records:add_mapping(?mapping(filter_service)),
	ok = openstack_records:add_mapping(?mapping(filter_role)),

	ok = openstack_records:add_mapping(?mapping(server)),
	ok = openstack_records:add_mapping(?mapping(changePassword)),
	ok = openstack_records:add_mapping(?mapping(reboot)),
	ok = openstack_records:add_mapping(?mapping(rebuild)),
	ok = openstack_records:add_mapping(?mapping(resize)),
	ok = openstack_records:add_mapping(?mapping(addFloatingIp)),
	ok = openstack_records:add_mapping(?mapping(addSecurityGroup)),
	ok = openstack_records:add_mapping(?mapping(removeSecurityGroup)),
	ok = openstack_records:add_mapping(?mapping(removeFloatingIp)),
	ok = openstack_records:add_mapping(?mapping(createImage)),
	ok = openstack_records:add_mapping(?mapping(snapshot)),
	ok = openstack_records:add_mapping(?mapping(keypair)),
	ok = openstack_records:add_mapping(?mapping(volume)),
	ok = openstack_records:add_mapping(?mapping(volumeAttachment)),
	ok = openstack_records:add_mapping(?mapping(security_group)),
	ok = openstack_records:add_mapping(?mapping(security_group_rule)),
	
	ok = openstack_records:add_mapping(?mapping(filter_server)),
	ok = openstack_records:add_mapping(?mapping(filter_nova_image)),
	ok = openstack_records:add_mapping(?mapping(filter_nova_volume)),
	
	ok = openstack_records:add_mapping(?mapping(ximage)),
	ok = openstack_records:add_mapping(?mapping(ximagemeta)),
	ok = openstack_records:add_mapping(?mapping(ximage_filter)),
	
	ok = openstack_records:add_mapping(?mapping(container_filter)),
	ok = openstack_records:add_mapping(?mapping(object_filter)),

	Response.

%% @private
%% @doc Stops the application.
-spec(stop(any()) -> ok).
stop(_State) ->
    ok.


%% Internal functions
start(Application) ->
	case application:start(Application) of
		{error, {not_started, Dependency}} ->
			case start(Dependency) of
				ok ->
					start(Application);
				Error ->
					Error
			end;
		Result ->
			Result
	end.
