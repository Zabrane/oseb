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

-record(auth, {passwordCredentials, apiAccessKeyCredentials, tenantId, tenantName}).
-record(user, {id, username, email, enabled, name, tenantId}).
-record(tenant, {id, name, description, enabled}).

%-record(service, {id, type, name, description}).
-record('OS-KSADM:service', {id, type, name, description}).
-record(role, {id, name, description}).

-record(passwordCredentials, {username, password}).
-record(apiAccessKeyCredentials, {accessKey, secretKey}).

%% Filters
-record(filter_service, {marker, limit}).
-record(filter_role, {serviceId, marker, limit}).
