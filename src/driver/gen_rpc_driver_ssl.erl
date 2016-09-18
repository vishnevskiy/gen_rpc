%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_driver_ssl).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(gen_rpc_driver).

%%% Include this library's name macro
-include("app.hrl").
%%% Include SSL macros
-include("ssl.hrl").
-include("tcp.hrl").
%%% Include helpful guard macros
-include("guards.hrl").

%%% Public API
-export([connect/1,
        listen/1,
        get_peer/1,
        send/2,
        activate_socket/1,
        authenticate_server/1,
        authenticate_client/3,
        copy_sock_opts/2,
        set_controlling_process/2,
        set_send_timeout/2,
        set_acceptor_opts/1]).

%%% ===================================================
%%% Public API
%%% ===================================================
%% Connect to a node
-spec connect(atom()) -> {ok, ssl:sslsocket()} | {error, term()}.
connect(Node) when is_atom(Node) ->
    Host = gen_rpc_helper:host_from_node(Node),
    Port = gen_rpc_helper:get_port_per_node(Node),
    ConnTO = gen_rpc_helper:get_connect_timeout(),
    SslOpts = merge_ssl_options(client, Node),
    case ssl:connect(Host, Port, SslOpts, ConnTO) of
        {ok, Socket} ->
            ok = lager:debug("event=connect_to_remote_server peer=\"~s\" socket=\"~p\" result=success", [Node, Socket]),
            {ok, Socket};
        {error, Reason} ->
            ok = lager:error("event=connect_to_remote_server peer=\"~s\" result=failure reason=\"~p\"", [Node, Reason]),
            {error, {badtcp,Reason}}
    end.


-spec listen(inet:port_number()) -> {ok, ssl:sslsocket()} | {error, term()}.
listen(Port) when is_integer(Port) ->
    SslOpts = merge_ssl_options(server, undefined),
    ssl:listen(Port, SslOpts).

-spec send(ssl:sslsocket(), binary()) -> ok | {error, term()}.
send(Socket, Data) when is_tuple(Socket), is_binary(Data) ->
    case ssl:send(Socket, Data) of
        {error, timeout} ->
            ok = lager:error("event=send_data_failed socket=\"~p\" reason=\"timeout\"", [Socket]),
            {error, {badtcp,send_timeout}};
        {error, Reason} ->
            ok = lager:error("event=send_data_failed socket=\"~p\" reason=\"~p\"", [Socket, Reason]),
            {error, {badtcp,Reason}};
        ok ->
            ok = lager:debug("event=send_data_succeeded socket=\"~p\"", [Socket]),
            ok
    end.

-spec activate_socket(ssl:sslsocket()) -> ok.
activate_socket(Socket) when is_tuple(Socket) ->
    ok = ssl:setopts(Socket, [{active,once}]),
    ok.

%% Authenticate to a server
-spec authenticate_server(ssl:sslsocket()) -> ok | {error, {badtcp | badrpc, term()}}.
authenticate_server(Socket) ->
    Cookie = erlang:get_cookie(),
    Packet = erlang:term_to_binary({gen_rpc_authenticate_connection, Cookie}),
    SendTO = gen_rpc_helper:get_send_timeout(undefined),
    RecvTO = gen_rpc_helper:get_call_receive_timeout(undefined),
    ok = set_send_timeout(Socket, SendTO),
    case ssl:send(Socket, Packet) of
        {error, Reason} ->
            ok = lager:error("event=authentication_connection_failed socket=\"~p\" reason=\"~p\"", [Socket, Reason]),
            ok = ssl:close(Socket),
            {error, {badtcp,Reason}};
        ok ->
            ok = lager:debug("event=authentication_connection_succeeded socket=\"~p\"", [Socket]),
            case ssl:recv(Socket, 0, RecvTO) of
                {ok, RecvPacket} ->
                    case erlang:binary_to_term(RecvPacket) of
                        gen_rpc_connection_authenticated ->
                            ok = lager:debug("event=connection_authenticated socket=\"~p\"", [Socket]),
                            ok;
                        {gen_rpc_connection_rejected, Reason} ->
                            ok = lager:error("event=authentication_rejected socket=\"~p\" reason=\"~s\"", [Socket, Reason]),
                            ok = ssl:close(Socket),
                            {error, {badrpc,Reason}};
                        _Else ->
                            ok = lager:error("event=authentication_reception_error socket=\"~p\" reason=\"invalid_payload\"", [Socket]),
                            ok = ssl:close(Socket),
                            {error, {badrpc,invalid_message}}
                    end;
                {error, Reason} ->
                    ok = lager:error("event=authentication_reception_failed socket=\"~p\" reason=\"~p\"", [Socket, Reason]),
                    ok = ssl:close(Socket),
                    {error, {badtcp,Reason}}
            end
    end.

%% Authenticate a connected client
-spec authenticate_client(ssl:sslsocket(), tuple(), binary()) -> ok | {error, {badtcp | badrpc, term()}}.
authenticate_client(Socket, Peer, Data) ->
    Cookie = erlang:get_cookie(),
    try erlang:binary_to_term(Data) of
        {gen_rpc_authenticate_connection, Node, Cookie} ->
            Cert = ssl:peercert(Socket),
            {SocketResponse, AuthResult} = case ssl_verify_hostname:verify_cert_hostname(Cert, Node) of
                {fail, AuthReason} ->
                    ok = lager:error("event=node_certificate_mismatch socket=\"~p\" peer=\"~s\" reason=\"~p\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer), AuthReason]),
                    {{gen_rpc_connection_rejected,node_certificate_mismatch}, {error,{badrpc,node_certificate_mismatch}}};
                {valid, _Hostname} ->
                    ok = lager:debug("event=certificate_validated socket=\"~p\" peer=\"~s\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer)]),
                    {gen_rpc_connection_authenticated, ok}
            end,
            Packet = erlang:term_to_binary(SocketResponse),
            case send(Socket, Packet) of
                {error, Reason} ->
                    ok = lager:error("event=transmission_failed socket=\"~p\" peer=\"~s\" reason=\"~p\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer), Reason]),
                    {error, {badtcp,Reason}};
                ok ->
                    ok = lager:debug("event=transmission_succeeded socket=\"~p\" peer=\"~s\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer)]),
                    ok = activate_socket(Socket),
                    AuthResult
            end;
        {gen_rpc_authenticate_connection, _Node, _IncorrectCookie} ->
            ok = lager:error("event=invalid_cookie_received socket=\"~p\" peer=\"~s\"",
                             [Socket, gen_rpc_helper:peer_to_string(Peer)]),
            Packet = erlang:term_to_binary({gen_rpc_connection_rejected, invalid_cookie}),
            ok = case send(Socket, Packet) of
                {error, Reason} ->
                    ok = lager:error("event=transmission_failed socket=\"~p\" peer=\"~s\" reason=\"~p\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer), Reason]);
                ok ->
                    ok = lager:debug("event=transmission_succeeded socket=\"~p\" peer=\"~s\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer)])
            end,
            {error, {badrpc,invalid_cookie}};
        OtherData ->
            ok = lager:debug("event=erroneous_data_received socket=\"~p\" peer=\"~s\" data=\"~p\"",
                             [Socket, gen_rpc_helper:peer_to_string(Peer), OtherData]),
            {error, {badrpc,erroneous_data}}
    catch
        error:badarg ->
            {error, {badtcp,corrupt_data}}
    end.

-spec copy_sock_opts(port(), port()) -> ok | {error, any()}.
copy_sock_opts(ListSock, AccSock) ->
    gen_rpc_helper:copy_sock_opts(ListSock, AccSock, ?ACCEPTOR_COPY_TCP_OPTS).

-spec get_peer(ssl:sslsocket()) -> {inet:ip4_address(), inet:port_number()}.
get_peer(Socket) when is_tuple(Socket) ->
    {ok, Peer} = ssl:peername(Socket),
    Peer.

-spec set_controlling_process(ssl:sslsocket(), pid()) -> ok | {error, term()}.
set_controlling_process(Socket, Pid) when is_tuple(Socket), is_pid(Pid) ->
    ssl:controlling_process(Socket, Pid).

-spec set_send_timeout(ssl:sslsocket(), timeout() | undefined) -> ok.
set_send_timeout(Socket, SendTO) when is_tuple(Socket) ->
    ok = ssl:setopts(Socket, [{send_timeout, gen_rpc_helper:get_send_timeout(SendTO)}]),
    ok.

-spec set_acceptor_opts(ssl:sslsocket()) -> ok.
set_acceptor_opts(Socket) when is_tuple(Socket) ->
    ok = ssl:setopts(Socket, [{send_timeout, gen_rpc_helper:get_send_timeout(undefined)}|?ACCEPTOR_DEFAULT_TCP_OPTS]),
    ok.

%%% ===================================================
%%% Private functions
%%% ===================================================
merge_ssl_options(client, Node) ->
    {ok, ExtraOpts} = application:get_env(?APP, ssl_client_options),
    DefaultOpts = lists:append(?SSL_DEFAULT_COMMON_OPTS, ?SSL_DEFAULT_CLIENT_OPTS),
    VerifyOpts = [{verify_fun, {fun ssl_verify_hostname:verify_fun/3,[{check_hostname,Node}]}}|DefaultOpts],
    gen_rpc_helper:merge_sockopt_lists(ExtraOpts, VerifyOpts);

merge_ssl_options(server, _Node) ->
    {ok, ExtraOpts} = application:get_env(?APP, ssl_server_options),
    DefaultOpts = lists:append(?SSL_DEFAULT_COMMON_OPTS, ?SSL_DEFAULT_SERVER_OPTS),
    gen_rpc_helper:merge_sockopt_lists(ExtraOpts, DefaultOpts).
