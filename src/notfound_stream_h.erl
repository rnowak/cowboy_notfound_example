-module(notfound_stream_h).

-behavior(cowboy_stream).

-export([
  init/3, terminate/3,
  data/4, info/3,
  early_error/5
]).

-define(LOG(X, Y), io:format(X, Y)).

-record(state, {
  next,
  redirect_url,
  handler,
  handler_opts
}).

init(StreamId, Req, Opts) ->
  ?LOG("===== init stream_id=~p, req=~p, opts=~p~n", [StreamId, Req, Opts]),
  {Commands, Next} = cowboy_stream:init(StreamId, Req, Opts),
  ?LOG("     commands=~p, next=~p~n", [Commands, Next]),
  Env = maps:get(env, Opts, #{}),
  Url = maps:get(notfound_redirect_url, Env, undefined),
  {Commands, #state{next=Next, redirect_url=Url}}.

%% This is on receiving the request body, and we do nothing with it.
data(StreamId, IsFin, Data, #state{next=Prev}=State) ->
  ?LOG("  === data stream_id=~p, is_fin=~p, data=~p, state=~p~n", [StreamId, IsFin, Data, State]),
  {Commands, Next} = cowboy_stream:data(StreamId, IsFin, Data, Prev),
  ?LOG("    = commands=~p, next=~p~n", [Commands, Next]),
  {Commands, State#state{next=Next}}.

%% This is triggered by calling cowboy_req:cast/2 in notfound_middleware:execute/2.
%% Just an example as I was curious how one could retrieve which handler was
%% used inside of a stream handler.
info(StreamId, {set_handler, Handler, HandlerOpts}, State) ->
  ?LOG("  === set_handler stream_id=~p, handler=~p, handler_opts=~p, state=~p~n",
       [StreamId, Handler, HandlerOpts, State]),

  %% We're egoistical and don't let further stream handlers down the chain know about this
  %% command as we ignore calling cowboy_stream:info/3, and instead just eat it and return
  %% an empty list of commands.
  {[], State#state{handler=Handler, handler_opts=HandlerOpts}};

%% If we have a response with a 404 code, we rewrite it to a 307 redirect, but
%% we could do any imaginable thing here.
info(StreamId, {response, 404, _, _}, #state{next=Prev, redirect_url=Url}=State)
    when is_binary(Url) ->
  ?LOG("  === 404 stream_id=~p, redirect_url=~p~n", [StreamId, Url]),
  RewriteInfo = {response, 307, #{<<"location">> => Url}, <<>>},
  {Commands, Next} = cowboy_stream:info(StreamId, RewriteInfo, Prev),
  {Commands, State#state{next=Next}};

%% Fall-through, if we didn't care to handle the command in any special way.
info(StreamId, Info, #state{next=Prev}=State) ->
  ?LOG("  === info stream_id=~p, info=~p, state=~p~n", [StreamId, Info, State]),
  {Commands, Next} = cowboy_stream:info(StreamId, Info, Prev),
  ?LOG("    = commands=~p, next=~p~n", [Commands, Next]),
  {Commands, State#state{next=Next}}.

terminate(StreamId, Reason, #state{next=Prev}=State) ->
  ?LOG("  === terminate stream_id=~p, reason=~p, state=~p~n", [StreamId, Reason, State]),
  Ret = cowboy_stream:terminate(StreamId, Reason, Prev),
  ?LOG("    = ret=~p~n", [Ret]),
  Ret.

early_error(StreamId, Reason, PartialReq, Resp, Opts) ->
  ?LOG("  === early_error stream_id=~p, reason=~p, partial_req=~p, resp=~p, opts=~p~n",
       [StreamId, Reason, PartialReq, Resp, Opts]),
  Ret = cowboy_stream:early_error(StreamId, Reason, PartialReq, Resp, Opts),
  ?LOG("    = ret=~p~n", [Ret]),
  Ret.
