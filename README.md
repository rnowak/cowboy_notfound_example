# notfound

A small exploration of Cowboy stream handlers and middlewares.

This is a fairly small example of how a stream handler can be set up to
perform a simple task.

Additionally, I was curious how the stream handler could obtain arbitrary
data regarding the request that isn't provided by default, such as which
handler the router is dispatching and its opts.

The stream handler is implemented in `notfound_stream_h` and the middleware in
  `notfound_middleware`.

# Running
`rebar3 shell` and visit `http://127.0.0.1:8080/` for a normal request that succeeds using `notfound_index_h` handler; `http://127.0.0.1:8080/file1.html` for a file served by `cowboy_static`; and `http://127.0.0.1:8080/anything-else-at-all` to trigger the 404 handling in `notfound_stream_h`.

There will be a fair amount of output on the console available to observe.

# License

Released under Apache License 2.0
