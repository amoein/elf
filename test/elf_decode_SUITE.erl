-module(elf_decode_SUITE).
-author("aleyandro").
-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([tcp_test/1, test_create_post/1]).

-include_lib("eunit/include/eunit.hrl").
-include("elf.hrl").

-record(xmlel,
{
  name ,
  attrs ,
  children
}).
-define(config(A,B) , lists:keyfind(A,1,B)).

suite() -> [].

init_per_suite(Config) ->
    application:ensure_started(elf),
    application:ensure_started(fast_xml),
    EL =
        [#xmlel{
            name = <<"post">>,
            children =
                [
                 #xmlel{name = <<"body">>, children = [{xmlcdata, <<"Test">>}]},
                 #xmlel{
                    name = <<"meta">>,
                    children = [
                                #xmlel{name = <<"mute">>, children = [{xmlcdata, <<"1">>}]},
                                #xmlel{
                                   name = <<"forward">>,
                                   children = [
                                               #xmlel{name = <<"item">>, children = [{xmlcdata, <<"AAAZKITbYxcAAAAD">>}]},
                                               #xmlel{name = <<"version">>, children = [{xmlcdata, <<"2">>}]}
                                              ]}
                               ]}]}],
    Post = #elf_post{body = <<"test">>,
                 meta = [#elf_create_date{time = 100100},
                         #elf_post_version{num = 1},
                         #elf_view_count{count = 0},
                         #elf_publisher{publisher = <<"self">>},
                         #elf_forward{item_id = <<"AAAZKITbYxcAAAAD">>, version = 2}]},

    [{test_element, EL}, {test_post, Post} | Config].


end_per_suite(_Config) ->
    ok.

all() ->
    [tcp_test, test_create_post].

tcp_test(Config) ->

  {_,EL} = ?config(test_element, Config),
    T = #elf_post{body = <<"Test">>,
              meta = [#elf_mute{mute = true},
                      #elf_forward{item_id = <<"AAAZKITbYxcAAAAD">>, version = 2}]},
    R = elf_codec_post:decode(EL),
    case T =:= R of true -> ok; false -> error end.

test_create_post(Config) ->
  {_, Post} = ?config(test_post, Config),
    Encode = elf_codec_post:encode(Post),
    ct:print("~p", [Encode]),
    ok.
