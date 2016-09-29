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
-behaviour(gen_statem).

%%% Include this library's name macro
-include("app.hrl").

%%% Local state
%%% Local state
-record(state, {socket :: port(),
        driver :: atom(),
        driver_mod :: atom()}).

%%% Server functions
-export([start_link/0, stop/0]).

%% gen_statem callbacks
-export([init/1, handle_event/4, callback_mode/0, terminate/3, code_change/4]).

%% State machine states
-export([waiting_for_connection/3]).

%%% ===================================================
%%% Supervisor functions
%%% ===================================================
-spec start_link() -> gen_statem:startlink_ret().
start_link() ->
    gen_statem:start_link({local,?MODULE}, ?MODULE, {}, []).

-spec stop() -> ok.
stop() ->
    gen_statem:stop(?MODULE, normal, infinity).

%%% ===================================================
%%% Behaviour callbacks
%%% ===================================================
init({}) ->
    ok = gen_rpc_helper:set_optimal_process_flags(),
    {ok, Port} = application:get_env(?APP, port),
    {Driver, DriverMod, _ClosedMsg, _ErrorMsg} = gen_rpc_helper:get_transport_driver(),
    case DriverMod:listen(Port) of
        {ok, Socket} ->
            %% Launch a new acceptor with a new accept socket
            ok = lager:info("event=server_setup_successfully driver=~s socket=\"~s\"", [Driver, gen_rpc_helper:socket_to_string(Socket)]),
            {ok, waiting_for_connection, #state{socket=Socket, driver=Driver, driver_mod=DriverMod}, {next_event, internal, accept}};
        {error, Reason} ->
            ok = lager:error("event=failed_to_setup_server driver=~s reason=\"~p\"", [Driver, Reason]),
            {stop, Reason}
    end.

callback_mode() ->
    state_functions.

waiting_for_connection(internal, accept, #state{socket=ListSock, driver=Driver, driver_mod=DriverMod} = State) ->
    case DriverMod:accept(ListSock) of
        {ok, AccSock} ->
            ok = lager:info("event=client_connection_received driver=~s socket=\"~s\" action=starting_acceptor",
                            [Driver, gen_rpc_helper:socket_to_string(ListSock)]),
            Peer = DriverMod:get_peer(AccSock),
            {ok, AccPid} = gen_rpc_acceptor_sup:start_child(Peer),
            case DriverMod:copy_sock_opts(ListSock, AccSock) of
                ok -> ok;
                {error, Reason} -> exit({set_sock_opt, Reason})
            end,
            ok = DriverMod:set_controlling_process(AccSock, AccPid),
            ok = gen_rpc_acceptor:set_socket(AccPid, AccSock),
            {keep_state_and_data, {next_event, internal, accept}};
        {error, Reason} ->
            ok = lager:error("event=socket_error_event driver=~s socket=\"~s\" event=\"~p\" action=stopping",
                             [Driver, gen_rpc_helper:socket_to_string(ListSock), Reason]),
            {stop, {socket_error, Reason}, State}
    end.

handle_event(EventType, Event, StateName, #state{socket=Socket, driver=Driver} = State) ->
    ok = lager:error("event=uknown_event driver=~s socket=\"~s\" event_type=\"~p\" payload=\"~p\" action=stopping",
                     [Driver, gen_rpc_helper:socket_to_string(Socket), EventType, Event]),
    {stop, {StateName, undefined_event, Event}, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {state_functions, StateName, State}.

%%% ===================================================
%%% Private functions
%%% ===================================================
