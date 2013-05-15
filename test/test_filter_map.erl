-module(test_filter_map).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("openstack.hrl").

%% records used for testing.
-record(foo, {bar, baz}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    openstack_records:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

nothing_to_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 "" = openstack_records:map_filter(#foo{})
     end}.

one_binary_value_to_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 "bar=3" = openstack_records:map_filter(#foo{bar= <<"3">>})
     end}.

one_string_value_to_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 "bar=3" = openstack_records:map_filter(#foo{bar= "3"})
     end}.

one_integer_value_to_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 "bar=5" = openstack_records:map_filter(#foo{bar=5})
     end}.

two_values_to_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 "bar=3&baz=4" = openstack_records:map_filter(#foo{bar= "3", baz = "4"})
     end}.

check_escape_chars_filter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 "bar=hello%2c%20world%21" = openstack_records:map_filter(#foo{bar= "hello, world!"})
     end}.

map_string_test() ->
	"hello=3" = openstack_records:map_filter("hello=3").

map_binary_test() ->
	"hello=3" = openstack_records:map_filter(<<"hello=3">>).
