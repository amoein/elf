-module(elf_config).

%% API
-export([get_env_required/1, get_env/2]).


get_env_required(Key) ->
  case application:get_env(elf, Key) of
    {ok, Val} -> Val;
    undefined ->
      erlang:error(undefined, Key)
  end.

get_env(Key, Default) ->
  case application:get_env(elf, Key) of
    {ok, Val} -> Val;
    undefined ->
      Default
  end.