-module(eric_parser_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
  {
    response,
    [
      <<"hitchcock.freenode.net">>,
      [],
      notice,
      <<"*">>,
      <<"*** Looking up your hostname...">>
   ]
  } = eric_parser:parse(":hitchcock.freenode.net NOTICE * :*** Looking up your hostname..."),

  {
    response,
    [
      <<>>,
      <<>>,
      ping,
      <<"hitchcock.freenode.net">>
    ]
  } = eric_parser:parse("PING :hitchcock.freenode.net").
