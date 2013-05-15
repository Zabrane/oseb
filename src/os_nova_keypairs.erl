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
%% @doc Functions for interacting with the key pairs extensions to Nova.
%%      

-module(os_nova_keypairs).

%% Includes
-include("os_nova.hrl").

-export([delete/2,
		 generate/2,
		 import/3,
		 list/1]).

%% API
%% @doc Deletes a key pair.
%%
%% @spec delete(pid(), string()) -> ok    
%% @end
delete(NovaPid, Name) ->
	openstack_server:delete(NovaPid, ["os-keypairs/", Name]).

%% @doc Generates a key pair and returns a PEM-encoded private key.
%%
%% @spec generate(pid(), string()) -> {ok, jsondoc()}    
%% @end
generate(NovaPid, Name) when is_binary(Name) ->
	KeyPair = #keypair{name=Name},
	openstack_server:post(NovaPid, "os-keypairs", KeyPair);
generate(NovaPid, Name) when is_list(Name) ->
	generate(NovaPid, list_to_binary(Name)).

%% @doc Imports the public key of a key pair.
%%
%% @spec import(pid(), string(), jsondoc()) -> {ok, jsondoc()}    
%% @end
import(NovaPid, Name, PublicKey) when is_binary(Name), is_binary(PublicKey) ->
	KeyPair = #keypair{name=Name, public_key=PublicKey},
	openstack_server:post(NovaPid, "os-keypairs", KeyPair);
import(NovaPid, Name, PublicKey) when is_list(Name) ->
	import(NovaPid, list_to_binary(Name), PublicKey);
import(NovaPid, Name, PublicKey) when is_list(PublicKey) ->
	import(NovaPid, Name, list_to_binary(PublicKey)).

%% @doc Lists all public keys.
%%
%% @spec list(pid()) -> {ok, jsondoc()}    
%% @end
list(NovaPid) ->
	openstack_server:get(NovaPid, "os-keypairs").
