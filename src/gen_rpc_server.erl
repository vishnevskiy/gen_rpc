%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%
%%% Original concept inspired and some code copied from
%%% https://erlangcentral.org/wiki/index.php?title=Building_a_Non-blocking_TCP_server_using_OTP_principles

-module(gen_rpc_server).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Behaviour
-behaviour(gen_server).

%%% Include this library's name macro
-include("app.hrl").

%%% Local state
-record(state, {socket :: port(),
        driver :: atom(),
        driver_mod :: atom(),
        acceptor :: prim_inet:insock()}).

%%% Supervisor functions
-export([start_link/0, stop/0]).

%%% Behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%%% ===================================================
%%% Supervisor functions
%%% ===================================================
-spec start_link() -> gen_sever:startlink_ret().
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, {}, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE, normal, infinity).

%%% ===================================================
%%% Behaviour callbacks
%%% ===================================================
init({}) ->
    ok = gen_rpc_helper:set_optimal_process_flags(),
    {ok, Port} = application:get_env(?APP, port),
    {Driver, DriverMod, _ClosedMsg, _ErrorMsg} = gen_rpc_helper:get_transport_driver(),
    case DriverMod:listen(Port) of
        {ok, Socket} ->
            ok = lager:info("event=server_setup_successfully driver=~s socket=\"~p\"", [Driver, Socket]),
            {ok, Ref} = prim_inet:async_accept(Socket, -1),
            {ok, #state{socket=Socket, driver=Driver, driver_mod=DriverMod, acceptor=Ref}};
        {error, Reason} ->
            ok = lager:error("event=failed_to_setup_server driver=~s reason=\"~p\"", [Driver, Reason]),
            {stop, Reason}
    end.

%% Catch-all for calls - die if we get a message we don't expect
handle_call(Msg, _From,  #state{socket=Socket, driver=Driver} = State) ->
    ok = lager:error("event=unknown_call_received driver=~s socket=\"~p\" message=\"~p\" action=stopping", [Driver, Socket, Msg]),
    {stop, {unknown_call, Msg}, {unknown_call, Msg}, State}.

%% Catch-all for casts - die if we get a message we don't expect
handle_cast(Msg, #state{socket=Socket, driver=Driver} = State) ->
    ok = lager:error("event=unknown_cast_received driver=~s socket=\"~p\" message=\"~p\" action=stopping", [Driver, Socket, Msg]),
    {stop, {unknown_cast, Msg}, State}.

handle_info({inet_async, ListSock, Ref, {ok, AccSock}},
            #state{socket=ListSock, driver=Driver, driver_mod=DriverMod, acceptor=Ref} = State) ->
    try
        ok = lager:info("event=client_connection_received driver=~s socket=\"~p\" action=starting_acceptor", [Driver, ListSock]),
        Peer = DriverMod:get_peer(AccSock),
        {ok, AccPid} = gen_rpc_acceptor_sup:start_child(Peer),
        case DriverMod:copy_sock_opts(ListSock, AccSock) of
            ok -> ok;
            {error, Reason} -> exit({set_sock_opt, Reason})
        end,
        ok = DriverMod:set_controlling_process(AccSock, AccPid),
        ok = gen_rpc_acceptor:set_socket(AccPid, AccSock),
        case prim_inet:async_accept(ListSock, -1) of
            {ok, NewRef} -> {noreply, State#state{acceptor=NewRef}, hibernate};
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end
    catch
        exit:ExitReason ->
            ok = lager:error("message=inet_async event=unknown_error driver=~s socket=\"~p\" error=\"~p\" action=stopping",
                             [Driver, ListSock, ExitReason]),
            {stop, ExitReason, State}
    end;

%% Handle async socket errors gracefully
handle_info({inet_async, ListSock, Ref, Error}, #state{socket=ListSock, driver=Driver, acceptor=Ref} = State) ->
    ok = lager:error("message=inet_async event=listener_error driver=~s socket=\"~p\" error=\"~p\" action=stopping", [Driver, ListSock, Error]),
    {stop, Error, State};

%% Catch-all for info - our protocol is strict so die!
handle_info(Msg, #state{socket=Socket, driver=Driver} = State) ->
    ok = lager:error("event=uknown_message_received driver=~s socket=\"~p\" message=\"~p\" action=stopping", [Driver, Socket, Msg]),
    {stop, {unknown_message,Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
