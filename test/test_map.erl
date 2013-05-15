%% Author: carl
%% Created: 05 Jan 2013
%% Description: TODO: Add description to test_map
-module(test_map).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("openstack.hrl").
-include_lib("os_keystone.hrl").

%% records used for testing.
-record(empty, {}).
-record(foo, {bar, baz}).
-record(nests_empty, {empty}).
-record(bar, {baz}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    openstack_records:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

add_ok_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo))
     end}.

add_ok2_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping({foo, [foo, bar]})
     end}.

add_not_ok_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ?assertError(_, openstack_records:add_mapping({[foo, bar]}))
     end}.

add_not_ok2_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ?assertError(_, openstack_records:add_mapping({"hello", [foo, bar]}))
     end}.

is_mapped_true_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 true = openstack_records:is_mapped({foo, 1, 2})
     end}.

is_mapped_false_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
		 false = openstack_records:is_mapped({foo, 1, 2})
     end}.

is_mapped_false2_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 false = openstack_records:is_mapped({foo, 1, 2, 3})
     end}.

is_mapped_false3_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 false = openstack_records:is_mapped("not a tuple")
     end}.

get_mapping_record_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 [bar, baz] = openstack_records:get_mapping(#foo{})
     end}.
	
map_binary_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
		 <<"123">> = openstack_records:map(<<"123">>)
     end}.

map_json_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  Json = {[{<<"foo">>, <<"bar">>}]},
			  json_eep:term_to_json(Json),
			  Json = openstack_records:map(Json)
     end}.

map_simple_list_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  List = [1, 2, 3],
			  List = openstack_records:map(List)
     end}.

map_number_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  5.2 = openstack_records:map(5.2)
     end}.

map_simple_record_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(empty)),
			  Expected = {[{<<"empty">>, {[]}}]},
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map(#empty{})
     end}.

map_simple2_record_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(foo)),
			  Expected = {[{<<"foo">>, {[]}}]},
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map(#foo{})
     end}.

map_record_with_entry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(foo)),
			  Expected = {[{<<"foo">>, {[{<<"bar">>, 3}]}}]},
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map(#foo{bar=3})
     end}.

map_record_with_entry_to_json_doc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(foo)),
			  ExpectedTerm = {[{<<"foo">>, {[{<<"bar">>, 3}]}}]},
			  Expected = json_eep:term_to_json(ExpectedTerm),
			  Expected = openstack_records:map_to_json(#foo{bar=3})
     end}.

map_record_with_entries_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(foo)),
			  Expected = {[{<<"foo">>, {[{<<"bar">>, <<"3">>}, {<<"baz">>, <<"4">>}]}}]},
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map(#foo{bar= <<"3">>, baz= <<"4">>})
     end}.

simpled_nested_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(nests_empty)),
			  ok = openstack_records:add_mapping(?mapping(empty)),
			  Expected = {[{<<"nests_empty">>, {[{<<"empty">>, {[]}}]}}]},
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map(#nests_empty{empty=#empty{}})
     end}.

array_record_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(empty)),
			  Expected = [{[{<<"empty">>, {[]}}]}],
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map([#empty{}])
     end}.


nested_array_record_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(foo)),
			  ok = openstack_records:add_mapping(?mapping(bar)),
			  Expected = {[{<<"foo">>,
						   	{[{
							   <<"bar">>, [
											{[{<<"bar">>,
											   {[]}
											}]}
										   ]
							}]}	
						   }]},
			  json_eep:term_to_json(Expected),
			  Expected = openstack_records:map(#foo{bar=[#bar{}]})
     end}.

access_key_endoding_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
			  ok = openstack_records:add_mapping(?mapping(apiAccessKeyCredentials)),
			  ok = openstack_records:add_mapping(?mapping(auth)),
			  Expected = {[{<<"auth">>,
							{[{<<"apiAccessKeyCredentials">>,
							   {[{<<"accessKey">>, <<"foo">>},
								 {<<"secretKey">>, <<"bar">>}]}
							  },
							  {<<"tenantId">>, <<"123">>}
							 ]}
						   }]},
			  json_eep:term_to_json(Expected),
			  ApiAccessCreds = #apiAccessKeyCredentials{accessKey= <<"foo">>, secretKey= <<"bar">>},
			  Expected = openstack_records:map(#auth{apiAccessKeyCredentials=ApiAccessCreds, tenantId= <<"123">>})
     end}.
	
map_json_to_json_test() ->
	JsonDoc = "{\"foo\": \"bar\"}",
	JsonDoc = openstack_records:map_to_json(JsonDoc).

map_term_to_json_test() ->
	Term = json_eep:json_to_term("{\"foo\": \"bar\"}"),
	JsonDoc = json_eep:term_to_json(Term),
	JsonDoc = openstack_records:map_to_json(Term).
