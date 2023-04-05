-module(elf_codec_post).
-author("aleyandro").

%% API
-export([decode/1, encode/1]).

-include("elf.hrl").

-record(xmlel, {name = <<"">> :: binary(),
                attrs :: list(),
                children :: list()}).
-type xmlel() :: #xmlel{}.
%%====================================================================
%% API functions
%%====================================================================
-spec decode(xmlel()) -> elf_post()|error.
decode(Payload) when is_list(Payload) ->
  [decode(Item) || Item <- Payload];

decode(Item) when is_record(Item, xmlel) ->
  decode(Item, #elf_post{});

decode(_) -> error(undefind_element).

-spec encode(elf_post()) -> xmlel().
encode(#elf_post{body = Body, meta = Meta}) ->
  #xmlel{name = <<"post">>, children = [encode_post_body(Body), encode_post_meta(Meta)]}.
%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Decode
%%====================================================================
decode(#xmlel{name = <<"post">>, children = Els}, Post) -> decode_post_elemnt(Els, Post);
decode(_, Post) -> Post.

decode_post_elemnt([#xmlel{name = <<"body">>, children = Els} | Rest], Post) ->
  Body = decode_post_body_elements(Els),
  decode_post_elemnt(Rest, Post#elf_post{body = Body});

decode_post_elemnt([#xmlel{name = <<"meta">>, children = Els} | Rest], Post) ->
  Meta = decode_post_meta_elements(Els, []),
  decode_post_elemnt(Rest, Post#elf_post{meta = Meta});

decode_post_elemnt([_ | Rest], Post) -> decode_post_elemnt(Rest, Post);
decode_post_elemnt([], Post) -> Post.

decode_post_body_elements([{xmlcdata, Data}]) -> Data;
decode_post_body_elements(_) -> error(decode_post_body_elements).

decode_post_meta_elements([Item | Rest], Meta) ->
  decode_post_meta_elements(Rest, decode_post_meta_element(Item, Meta));

decode_post_meta_elements([], Meta) -> Meta.

decode_post_meta_element(#xmlel{name = <<"mute">>, children = Els}, Meta) ->
  [#elf_mute{mute = decode_meta_mute_value(Els)} | Meta];

decode_post_meta_element(#xmlel{name = <<"forward">>, children = Els}, Meta) ->
  [decode_meta_forward_value(Els, #elf_forward{}) | Meta];
decode_post_meta_element(_, Meta) -> Meta.

decode_meta_mute_value([{xmlcdata, Data}]) -> if Data == <<"1">> -> true; true -> false end;
decode_meta_mute_value(_) -> false.

decode_meta_forward_value([#xmlel{name = <<"id">>, children = [{xmlcdata, Data}]} | Rest], Forward) ->
  decode_meta_forward_value(Rest, Forward#elf_forward{item_id = Data});

decode_meta_forward_value([#xmlel{name = <<"version">>, children = [{xmlcdata, Data}]} | Rest], Forward) ->
  decode_meta_forward_value(Rest, Forward#elf_forward{version = binary_to_integer(Data)});

decode_meta_forward_value([_ | Rest], Forward) -> decode_meta_forward_value(Rest, Forward);

decode_meta_forward_value([], Forward) -> Forward.


%%====================================================================
%% Encode
%%====================================================================

encode_post_body(Body) ->
  #xmlel{name = <<"body">>,
    attrs = [],
    children = [{xmlcdata, Body}]}.

encode_post_meta(Meta) ->
  #xmlel{name = <<"meta">>,
    attrs = [],
    children = [encode_post_meta_item(Item) || Item <- Meta, encode_post_meta_item(Item) =/= []]}.


encode_post_meta_item(#elf_view_count{count = Count}) ->
  #xmlel{name = <<"viewCount">>,
    attrs = [],
    children = [{xmlcdata, integer_to_binary(Count)}]};

encode_post_meta_item(#elf_post_version{num = NUM}) ->
  #xmlel{name = <<"version">>,
    attrs = [],
    children = [{xmlcdata, integer_to_binary(NUM)}]};

encode_post_meta_item(#elf_create_date{time = Time}) ->
  #xmlel{name = <<"create_date">>,
    attrs = [],
    children = [{xmlcdata, integer_to_binary(Time)}]};

encode_post_meta_item(#elf_publisher{publisher = Publisher}) ->
  #xmlel{name = <<"publisher">>,
    attrs = [],
    children = [{xmlcdata, Publisher}]};

encode_post_meta_item(#elf_forward{item_id = ItemID, version = Version}) ->
  #xmlel{name = <<"forward">>,
    attrs = [],
    children = [#xmlel{name = <<"id">>, children = [{xmlcdata, ItemID}]},
      #xmlel{name = <<"version">>, children = [{xmlcdata, integer_to_binary(Version)}]}]
  };

encode_post_meta_item(_) -> [].
