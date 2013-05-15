-module(test_header_map).

%% Include files
-include_lib("eunit/include/eunit.hrl").
-include_lib("openstack.hrl").

%% records used for testing.
-record(foo, {bar, baz, bat, fooBar}).

setup() ->
    T = ets:new(myets,[named_table,public]), 
    openstack_records:start_link(T). 

cleanup(_) ->
	ets:delete(myets).

nothing_to_map_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 [] = openstack_records:map_headers("x-", #foo{})
     end}.

one_field_to_map_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 [{"x-bar", "3"}] = openstack_records:map_headers("x-", #foo{bar="3"})
     end}.

two_fields_to_map_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 [{"x-bar", "3"}, {"x-baz", "a"}] = openstack_records:map_headers("x-", #foo{bar="3", baz="a"})
     end}.

one_int_field_to_map_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 [{"x-bar", "3"}] = openstack_records:map_headers("x-", #foo{bar=3})
     end}.

one_binary_field_to_map_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 [{"x-bar", "3"}] = openstack_records:map_headers("x-", #foo{bar= <<"3">>})
     end}.

unmap_no_headers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{} = openstack_records:unmap_headers(foo, "x-", [])
     end}.

unmap_header_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{bar= <<"hello">> } = openstack_records:unmap_headers(foo, "x-", [{"x-bar", "hello"}])
     end}.

unmap_unknown_header_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{bar= <<"hello">> } = openstack_records:unmap_headers(foo, "x-", [{"x-bar", "hello"}, {"x-unknown", "3"}])
     end}.

unmap_irrelevant_header_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{bar= <<"hello">> } = openstack_records:unmap_headers(foo, "x-", [{"x-bar", "hello"}, {"unknown", "3"}])
     end}.

unmap_two_headers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{bar= <<"hello">>, bat= <<"3">>} = openstack_records:unmap_headers(foo, "x-", [{"x-bar", "hello"}, {"x-bat", "3"}])
     end}.

unmap_uppercase_header_prefix_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{bar= <<"hello">> } = openstack_records:unmap_headers(foo, "x-", [{"X-bar", "hello"}])
     end}.

unmap_uppercase_header_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{bar= <<"Hello">> } = openstack_records:unmap_headers(foo, "x-", [{"X-BAR", "Hello"}])
     end}.

unmap_uppercase_record_name_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun () ->
	     ok = openstack_records:add_mapping(?mapping(foo)),
		 #foo{fooBar= <<"hello">> } = openstack_records:unmap_headers(foo, "x-", [{"x-foobar", "hello"}])
     end}.

filter_headers_basic_test() ->
	[{"x-foobar", "hello"}, {"x-a", "b"}] = openstack_records:filter_headers("x-", [{"x-foobar", "hello"}, {"x-a", "b"}]).

filter_headers_test() ->
	[{"x-foobar", "hello"}, {"x-a", "b"}] = openstack_records:filter_headers("x-", [{"x-foobar", "hello"}, {"y-a", "b"}, {"x-a", "b"}]).

filter_headers_case_test() ->
	[{"x-foobar", "hello"}, {"x-a", "b"}] = openstack_records:filter_headers("X-", [{"x-foobar", "hello"}, {"y-a", "b"}, {"x-a", "b"}]).

filter_headers_case2_test() ->
	[{"x-foobar", "hello"}, {"X-a", "b"}] = openstack_records:filter_headers("x-", [{"x-foobar", "hello"}, {"y-a", "b"}, {"X-a", "b"}]).
