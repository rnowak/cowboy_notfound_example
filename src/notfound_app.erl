-module(notfound_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      %% Just an index page...
      {"/", notfound_index_h, []},
      %% ... and some static file serving to keep things interesting (?).
      {"/[...]", cowboy_static, {priv_dir, notfound, "static", []}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    env => #{
      dispatch => Dispatch,
      %% Will be used by our stream handler
      notfound_redirect_url=><<"/">>
    },
    middlewares => [
      cowboy_router,
      % Another curiosity was how we can provide arbitrary data to our
      % stream handler. The middleware below lets it know the handler and
      % handler_opts that cowbow_router dispatched for this request.
      notfound_middleware,
      cowboy_handler
    ],
    stream_handlers => [
      % Putting our stream handler before the required,
      % default final cowboy stream handler.
      notfound_stream_h,
      cowboy_stream_h
    ]
  }),

  notfound_sup:start_link().

stop(_State) ->
  ok.
