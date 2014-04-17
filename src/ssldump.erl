-module(ssldump).
-behavior(ranch_protocol).

-include_lib("ssl/src/ssl_connection.hrl"). % for #state{}
-include_lib("ssl/src/ssl_api.hrl"). % for #sslsocket{}

-export([proxy/1, proxy/3]).
-export([start_link/4, init/5]).
-export([loop/4]).

-export([server/1, server/2]).
-export([ssl_acceptor/1]).

-define(CONTENT_TYPE_APPLICATION_DATA, 23).

proxy([LPortAtom, RHostAtom, RPortAtom]) when is_atom(LPortAtom), is_atom(RPortAtom), is_atom(RHostAtom) ->
  LPort = list_to_integer(atom_to_list(LPortAtom)),
  RHost = atom_to_list(RHostAtom),
  RPort = list_to_integer(atom_to_list(RPortAtom)),
  proxy(LPort, RHost, RPort).

proxy(LPort, RHost, RPort) ->
  application:ensure_all_started(ranch),
  ranch:start_listener(?MODULE, 10, ranch_tcp, [{port, LPort}], ?MODULE, [RHost, RPort]).


start_link(Ref, Socket, Transport, [RHost, RPort]) ->
  InitArgs = [Ref, Socket, Transport, RHost, RPort],
  {ok, spawn_link(?MODULE, init, InitArgs)}.

init(Ref, LSocket, Transport, RHost, RPort) ->
  {ok, RSocket} = Transport:connect(RHost, RPort, [binary, {active, false}]),
  ok = ranch:accept_ack(Ref),
  ID = io_lib:print(self()),
  SWorker = spawn_link(?MODULE, loop, [Transport, RSocket, LSocket, ID ++ " <<"]),
  ok = Transport:controlling_process(RSocket, SWorker),
  loop(Transport, LSocket, RSocket, ID ++ " >>").


loop(Transport, ReadSocket, WriteSocket, Label) ->
  {ContentType, Length, Header} = recv_header(Transport, ReadSocket, Label),
  {ok, Fragment} = Transport:recv(ReadSocket, Length, infinity),
  ok = Transport:send(WriteSocket, [Header, Fragment]),
  log_packet(ContentType, Header, Fragment, Label),
  loop(Transport, ReadSocket, WriteSocket, Label).

recv_header(Transport, Socket, Label) ->
  case Transport:recv(Socket, 5, infinity) of
    {ok, <<ContentType:8, _:16, Length:16>> = Header} ->
      {ContentType, Length, Header};
    {error, Error} ->
      log_shutdown(Label, Error),
      exit({shutdown, Error})
  end.

log_packet(?CONTENT_TYPE_APPLICATION_DATA, _Header, _Fragment, _Label) ->
  ok;
log_packet(_, Header, Fragment, Label) ->
  io:format("~s  ~120p~n", [Label, <<Header/binary, Fragment/binary>>]).

log_shutdown(Label, Error) ->
  io:format("~s shutdown: ~120p~n", [Label, Error]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  Dummy SSL acceptor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

server([PortStr, CertPrefix]) ->
  spawn_link(?MODULE, server, [list_to_integer(PortStr), CertPrefix]).

server(Port, CertPrefix) ->
  application:ensure_all_started(ssl),
  {ok, ListenSocket} = ssl:listen(Port, [{certfile, CertPrefix ++ ".cert"}, {keyfile, CertPrefix ++ ".key"}, {reuseaddr, true}, {ciphers, server_ciphers()}]),
  [start_acceptor(ListenSocket) || _ <- lists:seq(1, 3)],
  pool_watcher(ListenSocket).

server_ciphers() ->
  [{KE, C, H} || {KE, C, H} <- ssl:cipher_suites(),
    lists:sublist(atom_to_list(KE), 4) /= "ecdh" orelse H == sha].

pool_watcher(ListenSocket) ->
  receive
    {'DOWN', _, _, _, _} ->
      start_acceptor(ListenSocket)
  end,
  pool_watcher(ListenSocket).

start_acceptor(ListenSocket) ->
  spawn_monitor(?MODULE, ssl_acceptor, [ListenSocket]).

ssl_acceptor(ListenSocket) ->
  {ok, Socket} = ssl:transport_accept(ListenSocket),
  io:format("Accepting SSL in ~w~n", [self()]),
  ok = ssl:ssl_accept(Socket),
  print_state(Socket),
  {ok, _} = ssl:recv(Socket, 0, 10000),
  ok = ssl:send(Socket, "HTTP/1.1 401 Unauthorized\nContent-Length: 0\n\n"),
  ok = ssl:close(Socket).


print_state(#sslsocket{pid = Pid}) ->
  {State, #state{socket_options = Opts}} = sys:get_state(Pid),
  io:format("Socket state: ~w, options: ~120p~n", [State, Opts]).
