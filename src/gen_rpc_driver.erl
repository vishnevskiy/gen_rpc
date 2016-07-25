%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_driver).

-callback connect(atom()) -> {ok, port()} | {error, term()}.

-callback listen(inet:port_number()) -> {ok, port()} | {error, term()}.

-callback activate(port()) -> ok.

-callback send(port(), binary()) -> ok | {error, term()}.

-callback get_peer(port()) -> {inet:ip4_address(), inet:port_number()}.

-callback copy_sock_opts(port(), port()) -> ok | {error, any()}.

-callback set_controlling_process(port(), pid()) -> ok | {error, term()}.

-callback set_send_timeout(port(), timeout() | undefined) -> ok.

-callback set_acceptor_opts(port()) -> ok.

-ifdef(TEST).
%% Stub function to fool code coverage
-export([stub/0]).
stub() -> ok.
-endif.
