-module(elf_id).
-author("aleyandro").

%% API
-export([new_chan/0, new_post/2,
         decode_post_id/1, cuid_from_post_id/1,
         key_time/2]).

new_chan() ->
    T = list_to_integer(lists:reverse(integer_to_list(erlang:system_time(millisecond)))),
    Head = elf_config:get_env_required('channel.guid.head'),
    Route = 0,
    Reserve = 0,
    UUID = <<Head:10, Route:10, T:46, Reserve:6>>,
    base64:encode(UUID).

new_post(ChanId, Seq) ->
    ChannelID = base64:decode(ChanId),
    Time = erlang:system_time(second),
    Days = erlang:round(Time / 86400),
    base64:encode(<<ChannelID/binary, Days:16, Seq:32>>).

decode_post_id(Id) ->
    <<T:72, _:16, S:32>> = base64:decode(Id),
    {T, S}.

cuid_from_post_id(Id) ->
    <<T:72, _:48>> = base64:decode(Id),
    base64:encode(<<T:72>>).

-spec key_time(binary(), binary()) -> {ok, binary()}|error.
key_time(ChanId, Time) ->
    BS = erlang:byte_size(Time),
    if BS =/= 13 -> error;
       true ->
            try
                Itime = erlang:binary_to_integer(Time),
                ChannelID = base64:decode(ChanId),
                Days = erlang:round(Itime/86400000),
                {ok, base64:encode(<<ChannelID/binary, Days:16, 1:32>>)}
            catch
                _ -> error
            end
    end.
