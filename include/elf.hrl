%%%-------------------------------------------------------------------
%%% @author aleyandro
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2018 10:36 AM
%%%-------------------------------------------------------------------
-author("aleyandro").

-record(elf_post, {body = <<>> :: elf_post_body(),
                   meta = [] :: [elf_post_meta()]}).

-record(elf_create_date, {time :: time_stamp()}).
-record(elf_post_version, {num :: string()}).
-record(elf_view_count, {count :: integer()}).
-record(elf_publisher, {publisher :: binary()}).
-record(elf_mute, {mute :: boolean()}).
-record(elf_forward, {item_id :: binary(), version :: integer()}).

-type elf_post() :: #elf_post{}.
-type elf_post_body() :: binary().
-type time_stamp() :: non_neg_integer().
-type elf_create_date() :: #elf_create_date{}.
-type elf_post_version() :: #elf_post_version{}.
-type elf_view_count() :: #elf_view_count{}.
-type elf_publisher() :: #elf_publisher{}.
-type elf_mute() :: #elf_mute{}.
-type elf_forward() :: #elf_forward{}.

-type elf_post_meta() :: [elf_create_date()|
                          elf_post_version()|
                          elf_view_count()|
                          elf_publisher()|
                          elf_mute()|
                          elf_forward()].
