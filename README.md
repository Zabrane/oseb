oseb
====

oseb is an Erlang/OTP application for interacting with the OpenStack Keystone,
Nova, Swift, Glance and Cinder services. In addition, the following extensions are supported
<ul>
<li>The Keystone administration API.</li>
<li>The volumes, security groups, floating IPs and key pairs extensions to Nova.</li>
</ul> 
<br/>

The API
-------
The modules in the application fall into two categories:
<ul>
<li>Service modules. These have module names that start with "os_", e.g. os_nova, os_keystone, etc.</li>
<li>Low-level modules that provide HTTP, REST and utility functions. Users of the API shouldn't need to interact
with these modules.</li>
</ul> 
<br/>

Getting Started
---------------
The <code>openstack</code> application can be started by executing <code>application:start(openstack)</code>. This application
depends on the <code>inets</code>, <code>ssl</code> and <code>json_eep</code> applications (i.e. those three applications must
be started before starting <code>openstack</code>.

This example uses HP Cloud. If you don't already have access to an OpenStack cloud, you can consider creating an account there.

In the following code we show some basic interactions with OpenStack. In this example, we:
<ul>
<li>Authenticate against the Keystone service.</li>
<li>Spawn a process for interacting with Nova.</li>
<li>Launch a virtual machine.</li>
<li>Create a 1 GB volume.</li>
<li>Attach the volume to our running server.</li>
</ul>
```
1> openstack_app:start().
ok

2> {ok,K}=os_keystone:start("https://region-a.geo-1.identity.hpcloudsvc.com:35357/v2.0/", "username", "password", "tenantname").
{ok,<0.70.0>}

3> {ok,N}=os_nova:start(K).
{ok,<0.73.0>}

4> rr(os_nova).
[addFloatingIp,changePassword,createImage,filter_nova_image,
 filter_nova_volume,filter_server,keypair,personality,reboot,
 rebuild,removeFloatingIp,resize,server,snapshot]

5> os_nova:create_server(N, #server{name= <<"My first VM">>, flavorRef=101, imageRef=54021}).    
{ok,{[{<<"server">>,
       {[{<<"status">>,<<"BUILD(scheduling)">>},
         {<<"updated">>,<<"2013-02-10T14:46:33Z">>},
         {<<"hostId">>,<<>>},
         {<<"user_id">>,<<"xxxxxxxxxxxxxx">>},
         {<<"name">>,<<"My first VM">>},
         {<<"links">>,
          [{[{<<"href">>,<<"https://az-1.region-a.geo-1.compute."...>>},
             {<<"rel">>,<<"self">>}]},
           {[{<<"href">>,<<"https://az-1.region-a.geo-1.comp"...>>},
             {<<"rel">>,<<"bookmark">>}]}]},
         {<<"addresses">>,{[]}},
         {<<"tenant_id">>,<<"xxxxxxxxxxxxxxxx">>},
         {<<"image">>,
          {[{<<"id">>,<<"54021">>},
            {<<"links">>,
             [{[{<<"href">>,<<"http"...>>},{<<"rel">>,<<...>>}]}]}]}},
         {<<"created">>,<<"2013-02-10T14:46:33Z">>},
         {<<"uuid">>,<<"be7c13c5-29d5-4ce3-b7f0-89e8e5b8f082">>},
         {<<"accessIPv4">>,<<>>},
         {<<"accessIPv6">>,<<>>},
         {<<"key_name">>,null},
         {<<"adminPass">>,<<"N39wSkknPewqvbMT">>},
         {<<"flavor">>,{[{<<"id">>,<<...>>},{<<...>>,...}]}},
         {<<"config_drive">>,<<>>},
         {<<"id">>,800679},
         {<<"security"...>>,[{...}]},
         {<<"meta"...>>,{...}}]}}]}}

6> rr(os_nova_volumes).             
[volume,volumeAttachment]

7> os_nova_volumes:create(N, #volume{size=1}).       
{ok,{[{<<"volume">>,
       {[{<<"status">>,<<"creating">>},
         {<<"displayDescription">>,null},
         {<<"availabilityZone">>,<<"nova">>},
         {<<"displayName">>,null},
         {<<"attachments">>,[{[]}]},
         {<<"volumeType">>,null},
         {<<"snapshotId">>,<<>>},
         {<<"size">>,1},
         {<<"id">>,90743},
         {<<"createdAt">>,<<"2013-02-10 14:48:05">>},
         {<<"metadata">>,{[]}}]}}]}}

8> os_nova_volumes:attach(N, "800679", #volumeAttachment{volumeId=90743, device= <<"/dev/vdc">>}).
{ok,{[{<<"volumeAttachment">>,
       {[{<<"id">>,90743},{<<"volumeId">>,90743}]}}]}}
```

