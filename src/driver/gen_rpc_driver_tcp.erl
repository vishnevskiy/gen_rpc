%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_driver_tcp).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(gen_rpc_driver).

%%% Include this library's name macro
-include("app.hrl").
%%% Include TCP macros
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
-spec connect(atom()) -> {ok, port()} | {error, term()}.
connect(Node) when is_atom(Node) ->
    Host = gen_rpc_helper:host_from_node(Node),
    Port = gen_rpc_helper:get_port_per_node(Node),
    ConnTO = gen_rpc_helper:get_connect_timeout(),
    case gen_tcp:connect(Host, Port, ?TCP_DEFAULT_OPTS, ConnTO) of
        {ok, Socket} ->
            ok = lager:debug("event=connect_to_remote_server peer=\"~s\" socket=\"~p\" result=success", [Node, Socket]),
            {ok, Socket};
        {error, Reason} ->
            ok = lager:error("event=connect_to_remote_server peer=\"~s\" result=failure reason=\"~p\"", [Node, Reason]),
            {error, {badtcp,Reason}}
    end.

-spec listen(inet:port_number()) -> {ok, port()} | {error, term()}.
listen(Port) when is_integer(Port) ->
    gen_tcp:listen(Port, ?TCP_DEFAULT_OPTS).

-spec activate_socket(port()) -> ok.
activate_socket(Socket) when is_port(Socket) ->
    ok = inet:setopts(Socket, [{active,once}]),
    ok.

-spec send(port(), binary()) -> ok | {error, term()}.
send(Socket, Data) when is_port(Socket), is_binary(Data) ->
    case gen_tcp:send(Socket, Data) of
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

%% Authenticate to a server
-spec authenticate_server(port()) -> ok | {error, {badtcp | badrpc, term()}}.
authenticate_server(Socket) ->
    Cookie = erlang:get_cookie(),
    Packet = erlang:term_to_binary({gen_rpc_authenticate_connection, Cookie}),
    SendTO = gen_rpc_helper:get_send_timeout(undefined),
    RecvTO = gen_rpc_helper:get_call_receive_timeout(undefined),
    ok = set_send_timeout(Socket, SendTO),
    case gen_tcp:send(Socket, Packet) of
        {error, Reason} ->
            ok = lager:error("event=authentication_connection_failed socket=\"~p\" reason=\"~p\"", [Socket, Reason]),
            ok = gen_tcp:close(Socket),
            {error, {badtcp,Reason}};
        ok ->
            ok = lager:debug("event=authentication_connection_succeeded socket=\"~p\"", [Socket]),
            case gen_tcp:recv(Socket, 0, RecvTO) of
                {ok, RecvPacket} ->
                    case erlang:binary_to_term(RecvPacket) of
                        gen_rpc_connection_authenticated ->
                            ok = lager:debug("event=connection_authenticated socket=\"~p\"", [Socket]),
                            ok;
                        {gen_rpc_connection_rejected, invalid_cookie} ->
                            ok = lager:error("event=authentication_rejected socket=\"~p\" reason=\"invalid_cookie\"", [Socket]),
                            ok = gen_tcp:close(Socket),
                            {error, {badrpc,invalid_cookie}};
                        _Else ->
                            ok = lager:error("event=authentication_reception_error socket=\"~p\" reason=\"invalid_payload\"", [Socket]),
                            ok = gen_tcp:close(Socket),
                            {error, {badrpc,invalid_message}}
                    end;
                {error, Reason} ->
                    ok = lager:error("event=authentication_reception_failed socket=\"~p\" reason=\"~p\"", [Socket, Reason]),
                    ok = gen_tcp:close(Socket),
                    {error, {badtcp,Reason}}
            end
    end.

%% Authenticate a connected client
-spec authenticate_client(port(), tuple(), binary()) -> ok | {error, {badtcp | badrpc, term()}}.
authenticate_client(Socket, Peer, Data) ->
    Cookie = erlang:get_cookie(),
    try erlang:binary_to_term(Data) of
        {gen_rpc_authenticate_connection, Cookie} ->
            Packet = erlang:term_to_binary(gen_rpc_connection_authenticated),
            Result = case send(Socket, Packet) of
                {error, Reason} ->
                    ok = lager:error("event=transmission_failed socket=\"~p\" peer=\"~s\" reason=\"~p\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer), Reason]),
                    {error, {badtcp,Reason}};
                ok ->
                    ok = lager:debug("event=transmission_succeeded socket=\"~p\" peer=\"~s\"",
                                     [Socket, gen_rpc_helper:peer_to_string(Peer)]),
                    ok = activate_socket(Socket),
                    ok
            end,
            Result;
        {gen_rpc_authenticate_connection, _IncorrectCookie} ->
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

-spec get_peer(port()) -> {inet:ip4_address(), inet:port_number()}.
get_peer(Socket) when is_port(Socket) ->
    {ok, Peer} = inet:peername(Socket),
    Peer.

-spec set_controlling_process(port(), pid()) -> ok | {error, term()}.
set_controlling_process(Socket, Pid) when is_port(Socket), is_pid(Pid) ->
    gen_tcp:controlling_process(Socket, Pid).

-spec set_send_timeout(port(), timeout() | undefined) -> ok.
set_send_timeout(Socket, SendTO) when is_port(Socket) ->
    ok = inet:setopts(Socket, [{send_timeout, gen_rpc_helper:get_send_timeout(SendTO)}]),
    ok.

-spec set_acceptor_opts(port()) -> ok.
set_acceptor_opts(Socket) when is_port(Socket) ->
    ok = inet:setopts(Socket, [{send_timeout, gen_rpc_helper:get_send_timeout(undefined)}|?ACCEPTOR_DEFAULT_TCP_OPTS]),
    ok.

%%% ===================================================
%%% Private functions
%%% ===================================================
