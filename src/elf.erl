-module(elf).
-author("aleyandro").

%% API
-export([decode_item_payload/1,
         encode_item_post/1,
         new_channel_id/0,
         new_item_id/2,
         item_key_time/2,
         decode_post_id/1,
         cuid_from_post_id/1]).

-include("elf.hrl").

decode_item_payload(Payload) ->
    case elf_codec_post:decode(Payload) of
        error -> error;
        #elf_post{body = undefined} -> error;
        T -> {ok, T}
    end.

encode_item_post(Post) ->
    elf_codec_post:encode(Post).

new_channel_id() ->
    elf_id:new_chan().

new_item_id(NodeId, Sequnce) ->
    elf_id:new_post(NodeId, Sequnce).

decode_post_id(Id) ->
    elf_id:decode_post_id(Id).

cuid_from_post_id(Id) ->
    elf_id:cuid_from_post_id(Id).

item_key_time(NodeId, Time) ->
    elf_id:key_time(NodeId, Time).