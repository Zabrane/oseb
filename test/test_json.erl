-module(test_json).

%% Include files
-include_lib("eunit/include/eunit.hrl").

empty_list_test() ->
	Doc = [],
	json_eep:term_to_json(Doc),
	[] = openstack_json:get_values(Doc, <<"bar">>).

get_undefined_value_test() ->
	Doc = [],
	json_eep:term_to_json(Doc),
	undefined = openstack_json:get_first_value(Doc, <<"bar">>).

empty_result_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}]},
	json_eep:term_to_json(Doc),
	[] = openstack_json:get_values(Doc, <<"bar">>).

string_result_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}]},
	json_eep:term_to_json(Doc),
	[<<"bar">>] = openstack_json:get_values(Doc, "foo").

list_result_test() ->
	Doc = {[{<<"foo">>, [1, 2 ,3]}]},
	json_eep:term_to_json(Doc),
	[[1, 2, 3]] = openstack_json:get_values(Doc, <<"foo">>).

multiple_keys_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}, {<<"key">>, <<"value">>}]},
	json_eep:term_to_json(Doc),
	[<<"bar">>] = openstack_json:get_values(Doc, <<"foo">>),
	[<<"value">>] = openstack_json:get_values(Doc, <<"key">>).

multiple_docs_test() ->
	Doc = [ {[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"baz">>}]} ],
	json_eep:term_to_json(Doc),
	[<<"bar">>, <<"baz">>] = openstack_json:get_values(Doc, <<"foo">>).

get_only_value_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}]},
	json_eep:term_to_json(Doc),
	<<"bar">> = openstack_json:get_first_value(Doc, <<"foo">>),
	<<"bar">> = openstack_json:get_first_value([Doc], <<"foo">>).

get_first_value_test() ->
	Doc = [ {[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"baz">>}]} ],
	json_eep:term_to_json(Doc),
	<<"bar">> = openstack_json:get_first_value(Doc, <<"foo">>).

multiple_docs2_test() ->
	Doc = [ {[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"baz">>}]} , {[{<<"foo2">>, <<"baz">>}]} ],
	json_eep:term_to_json(Doc),
	[<<"bar">>, <<"baz">>] = openstack_json:get_values(Doc, <<"foo">>).

nested_doc_test() ->
	Doc = {[{<<"foo">>, {[{<<"bar">>, <<"baz">>}]} }]},
	json_eep:term_to_json(Doc),
	[<<"baz">>] = openstack_json:get_values(Doc, <<"foo.bar">>),
	[<<"baz">>] = openstack_json:get_values(Doc, "foo.bar").

triple_nested_doc_test() ->
	Doc = {[{<<"foo">>, {[{<<"bar">>, <<"baz">>}, {<<"baz">>, {[{<<"bar">>, <<"1">>}]}} ]} }]},
	json_eep:term_to_json(Doc),
	[{[{<<"bar">>, <<"baz">>}, {<<"baz">>, {[{<<"bar">>, <<"1">>}]}}]}] = openstack_json:get_values(Doc, <<"foo">>),
	[{[{<<"bar">>, <<"1">>}]}] = openstack_json:get_values(Doc, <<"foo.baz">>),
	[<<"1">>] = openstack_json:get_values(Doc, <<"foo.baz.bar">>),
	[<<"1">>] = openstack_json:get_values(Doc, <<"foo.baz.bar">>).

list_test() ->
	Doc = [{[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"baz">>}]}],
	json_eep:term_to_json(Doc),
	[<<"bar">>, <<"baz">>] = openstack_json:get_values(Doc, "foo").

empty_list2_test() ->
	Doc = [{[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"baz">>}]}],
	json_eep:term_to_json(Doc),
	[] = openstack_json:get_values(Doc, "foo.barx").

get_from_list_test() ->
	Doc = {[{<<"foo">>, [ {[{<<"a">>, <<"1">>}]}, {[{<<"b">>, <<"2">>}]}, {[{<<"a">>, <<"3">>}]}, 4, {[{<<"c">>,  {[{<<"d">>, 5}]}}]}]} ]},
	json_eep:term_to_json(Doc),
	[5] = openstack_json:get_values(Doc, "foo.c.d"),
	[<<"2">>] = openstack_json:get_values(Doc, "foo.b"),
	[<<"1">>, <<"3">>] = openstack_json:get_values(Doc, "foo.a").

get_from_ok_list_test() ->
	OkDoc = {ok, {[{<<"foo">>, [ {[{<<"a">>, <<"1">>}]}, {[{<<"b">>, <<"2">>}]}, {[{<<"a">>, <<"3">>}]}, 4, {[{<<"c">>,  {[{<<"d">>, 5}]}}]}]} ]}},
	[<<"1">>, <<"3">>] = openstack_json:get_values(OkDoc, "foo.a").

cant_get_from_list_test() ->
	Doc = {[{<<"foo">>, [1, 2, 3]} ]},
	json_eep:term_to_json(Doc),
	[[1, 2, 3]] = openstack_json:get_values(Doc, "foo"),
	[] = openstack_json:get_values(Doc, "foo.a").

weird_list_get_test() ->
	Doc = [{[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"bar">>}]}],
	json_eep:term_to_json(Doc),
	[<<"bar">>, <<"bar">>] = openstack_json:get_values(Doc, "foo").
	
select_not_found_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}]},
	json_eep:term_to_json(Doc),
	[] = openstack_json:select_values(Doc, {<<"foo">>, <<"baz">>}),
	[] = openstack_json:select_values(Doc, {<<"fooo">>, <<"bar">>}).

simple_select_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}]},
	json_eep:term_to_json(Doc),
	[Doc] = openstack_json:select_values(Doc, {<<"foo">>, <<"bar">>}).

simple_list_select_test() ->
	Doc = [{[{<<"foo">>, <<"bar">>}]}, {[{<<"foo">>, <<"bar">>}]}],
	json_eep:term_to_json(Doc),
	Doc = openstack_json:select_values(Doc, {<<"foo">>, <<"bar">>}).

less_simple_select_test() ->
	Doc = {[{<<"foo">>, <<"bar">>}, {<<"foo2">>, <<"bar2">>}]},
	json_eep:term_to_json(Doc),
	[Doc] = openstack_json:select_values(Doc, {<<"foo">>, <<"bar">>}),
	[Doc] = openstack_json:select_values(Doc, {<<"foo2">>, <<"bar2">>}).

nested_select_test() ->
	Doc = {[{<<"foo">>, {[{<<"bar">>, <<"baz">>}, {<<"baz">>, {[{<<"bar">>, <<"1">>}]}} ]} }]},
	json_eep:term_to_json(Doc),
	[{[{<<"bar">>, <<"1">>}]}] = openstack_json:select_values(Doc, {<<"foo.baz.bar">>, <<"1">>}), 
	[{[{<<"bar">>, <<"baz">>}, {<<"baz">>, {[{<<"bar">>, <<"1">>}]}} ]}] = openstack_json:select_values(Doc, {<<"foo.bar">>, <<"baz">>}). 

nested_ok_select_test() ->
	OkDoc = {ok, {[{<<"foo">>, {[{<<"bar">>, <<"baz">>}, {<<"baz">>, {[{<<"bar">>, <<"1">>}]}} ]} }]}},
	[{[{<<"bar">>, <<"baz">>}, {<<"baz">>, {[{<<"bar">>, <<"1">>}]}} ]}] = openstack_json:select_values(OkDoc, {<<"foo.bar">>, <<"baz">>}). 

nested_select_multiple_matches_test() ->
	Doc1 = {[{<<"a">>, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>} ]} }]},
	Doc2 = {[{<<"a">>, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"2">>} ]} }]},
	Doc = [Doc1, Doc2],
	json_eep:term_to_json(Doc),
	[{[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>}]}, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"2">>}]}] = openstack_json:select_values(Doc, {<<"a.b">>, <<"1">>}),
	[{[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>}]}] = openstack_json:select_values(Doc, {<<"a.c">>, <<"1">>}). 

nested_list_multiple_matches_test() ->
	Doc = {[{<<"a">>, [ {[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>} ]}, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"2">>} ]}] }]},
	json_eep:term_to_json(Doc),
	[{[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>}]}, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"2">>}]}] = openstack_json:select_values(Doc, {<<"a.b">>, <<"1">>}),
	[{[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>}]}] = openstack_json:select_values(Doc, {<<"a.c">>, <<"1">>}). 

select_first_value_test() ->
	Doc1 = {[{<<"a">>, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>} ]} }]},
	Doc2 = {[{<<"a">>, {[{<<"b">>, <<"1">>}, {<<"c">>, <<"2">>} ]} }]},
	Doc = [Doc1, Doc2],
	json_eep:term_to_json(Doc),
	{[{<<"b">>, <<"1">>}, {<<"c">>, <<"1">>}]} = openstack_json:select_first_value(Doc, {<<"a.b">>, <<"1">>}).

list_of_lists_test() ->
	Doc = [[{[{<<"b">>,<<"1">>}]}],[{[{<<"b">>,<<"2">>}]}]],
	[<<"1">>, <<"2">>] = openstack_json:get_values(Doc, "b").
