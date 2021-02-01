-module(notfound_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

-define(LOG(X, Y), io:format(X, Y)).

%% This middleware just informs our stream handler of the handler and
%% handler_opts env vars that the router set.
execute(Req, Env) ->
  ?LOG("  === middleware req=~p, env=~p~n", [Req, Env]),
  Handler = maps:get(handler, Env, undefined),
  HandlerOpts = maps:get(handler_opts, Env, undefined),
  cowboy_req:cast({set_handler, Handler, HandlerOpts}, Req),
  {ok, Req, Env}.
