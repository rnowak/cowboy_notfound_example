-module(notfound_index_h).

-export([init/2]).

init(Req0, Opts) ->
  Req = cowboy_req:reply(200, #{}, <<"Hello World!">>, Req0),
  {ok, Req, Opts}.
