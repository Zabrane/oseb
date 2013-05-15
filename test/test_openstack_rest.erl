-module(test_openstack_rest).

%% Include files
-include_lib("eunit/include/eunit.hrl").

code_204_test() ->
	ok = openstack_rest:format_response(204, "Phrase", [], <<>>).

code_200_test() ->
	{ok, <<"Hello">>} = openstack_rest:format_response(200, "Phrase", [], <<"Hello">>).
	
code_200_not_json_test() ->
	{ok, <<"Hello">>} = openstack_rest:format_response(200, "Phrase", [{"foo", "bar"}], <<"Hello">>).
	
code_404_test() ->
	{error, {404, "Not found"}} = openstack_rest:format_response(404, "Not found", [], <<"Hello">>).

code_200_json_data_test() ->
	{ok, {[{<<"foo">>, <<"bar">>}]}} = openstack_rest:format_response(200, "Phrase", [{"Content-Type", "application/json; foobar"}], <<"{\"foo\" : \"bar\"}">>).

code_200_json_data_2_test() ->
	{ok, {[{<<"foo">>, <<"bar">>}]}} = openstack_rest:format_response(200, "Phrase", [{"foo", "bar"}, {"Content-Type", "application/json; foobar"}], <<"{\"foo\" : \"bar\"}">>).

code_404_json_test() ->
	{error, {404, {[{<<"foo">>, <<"bar">>}]}}} = openstack_rest:format_response(404, "Phrase", [{"foo", "bar"}, {"Content-Type", "application/json; foobar"}], <<"{\"foo\" : \"bar\"}">>).

get_default_content_type_test() ->
	"foo" = openstack_rest:get_content_type([{"bar", "baz"}], "foo").

extract_content_type_test() ->
	"text" = openstack_rest:get_content_type([{"bar", "baz"}, {"content-type", "text"}], "bar").

extract_content_type_case_insensitive_test() ->
	"TEXT" = openstack_rest:get_content_type([{"bar", "baz"}, {"Content-Type", "TEXT"}], "baz").
