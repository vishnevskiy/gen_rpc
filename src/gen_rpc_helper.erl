%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%% Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
%%%

-module(gen_rpc_helper).
-author("Panagiotis Papadomitsos <pj@ezgr.net>").

%%% Include this library's name macro
-include("app.hrl").

%%% Public API
-export([peer_to_string/1,
        host_from_node/1,
        copy_sock_opts/3,
        set_optimal_process_flags/0,
        make_process_name/2,
        extract_node_name/1,
        merge_sockopt_lists/2,
        get_transport_driver/0,
        get_transport_driver_per_node/1,
        get_port_per_node/1,
        get_connect_timeout/0,
        get_send_timeout/1,
        get_call_receive_timeout/1,
        get_sbcast_receive_timeout/0,
        get_control_receive_timeout/0,
        get_inactivity_timeout/1,
        get_async_call_inactivity_timeout/0]).

%%% ===================================================
%%% Public API
%%% ===================================================
%% Return the connected peer's IP
-spec peer_to_string({inet:ip4_address(), inet:port_number()} | inet:ip4_address()) -> string().
peer_to_string({{A,B,C,D}, Port}) when is_integer(A), is_integer(B), is_integer(C), is_integer(D), is_integer(Port) ->
    lists:flatten([integer_to_list(A), ".",
    integer_to_list(B), ".",
    integer_to_list(C), ".",
    integer_to_list(D), ":",
    integer_to_list(Port)]);
peer_to_string({A,B,C,D} = IpAddress) when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    peer_to_string({IpAddress, 0}).

%% Return the remote Erlang hostname
-spec host_from_node(node()) -> string().
host_from_node(Node) when is_atom(Node) ->
    NodeStr = atom_to_list(Node),
    [_Name, Host] = string:tokens(NodeStr, [$@]),
    Host.

%% Set optimal process flags
-spec set_optimal_process_flags() -> ok.
set_optimal_process_flags() ->
    _ = erlang:process_flag(trap_exit, true),
    _ = erlang:process_flag(priority, high),
    _ = erlang:process_flag(message_queue_data, off_heap),
    ok.

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new acceptor socket.
-spec copy_sock_opts(port(), port(), list()) -> ok | {error, any()}.
copy_sock_opts(SourceSock, DestSock, Options) when is_port(SourceSock), is_port(DestSock), is_list(Options) ->
    true = inet_db:register_socket(DestSock, inet_tcp),
    case prim_inet:getopts(SourceSock, Options) of
        {ok, SockOpts} ->
            case prim_inet:setopts(DestSock, SockOpts) of
                ok -> ok;
                Error -> Error
            end;
        Error ->
            Error
        end.

%% Return an atom to identify gen_rpc processes
-spec make_process_name(list(), {inet:ip4_address(), inet:port_number()} | atom()) -> atom().
make_process_name("client", Node) when is_atom(Node) ->
    %% This function is going to be called enough to warrant a less pretty
    %% process name in order to avoid calling costly functions
    NodeStr = erlang:atom_to_list(Node),
    list_to_atom(lists:flatten(["gen_rpc.client.", NodeStr]));

make_process_name(Prefix, Peer) when is_list(Prefix), is_tuple(Peer) ->
    list_to_atom(lists:flatten(["gen_rpc.", Prefix, ".", peer_to_string(Peer)])).

%% Extract the node name from a gen_rpc client process name
-spec extract_node_name(atom()) -> atom().
extract_node_name(PidName) when is_atom(PidName) ->
    %% The process name follows the convention
    %% gen_rpc.client.(node name) which is 15 chars long
    PidStr = atom_to_list(PidName),
    list_to_atom(lists:nthtail(15, PidStr)).

%% Merge lists that contain both tuples and simple values observing
%% keys in proplists
-spec merge_sockopt_lists(list(), list()) -> list().
merge_sockopt_lists(List1, List2) ->
    SList1 = lists:usort(fun hybrid_proplist_compare/2, List1),
    SList2 = lists:usort(fun hybrid_proplist_compare/2, List2),
    lists:umerge(fun hybrid_proplist_compare/2, SList1, SList2).

%% Return the connection mode and helper module
-spec get_transport_driver() -> tuple().
get_transport_driver() ->
    {ok, Driver} = application:get_env(?APP, transport_driver),
    get_transport_driver_data_tuple(Driver).

-spec get_transport_driver_per_node(atom()) -> tuple().
get_transport_driver_per_node(Node) ->
    {ok, Nodes} = application:get_env(?APP, transport_driver_per_node),
    case maps:find(Node, Nodes) of
        error ->
            get_transport_driver();
        {ok, Driver} ->
            get_transport_driver_data_tuple(Driver)
    end.

%% Retrieves the specific TCP server port
-spec get_port_per_node(atom()) -> inet:port_number().
get_port_per_node(Node) ->
    {ok, Ports} = application:get_env(?APP, port_per_node),
    case maps:find(Node, Ports) of
        error ->
            {ok, Port} = application:get_env(?APP, port),
            Port;
        {ok, Port} ->
            Port
    end.

%% Retrieves the default connect timeout
-spec get_connect_timeout() -> timeout().
get_connect_timeout() ->
    {ok, ConnTO} = application:get_env(?APP, connect_timeout),
    ConnTO.

%% Merges user-defined call receive timeout values with app timeout values
-spec get_call_receive_timeout(undefined | timeout()) -> timeout().
get_call_receive_timeout(undefined) ->
    {ok, RecvTO} = application:get_env(?APP, call_receive_timeout),
    RecvTO;

get_call_receive_timeout(Else) ->
    Else.

%% Returns the default sbcast receive timeout
-spec get_sbcast_receive_timeout() -> timeout().
get_sbcast_receive_timeout() ->
    {ok, RecvTO} = application:get_env(?APP, sbcast_receive_timeout),
    RecvTO.

%% Returns the default dispatch receive timeout
-spec get_control_receive_timeout() -> timeout().
get_control_receive_timeout() ->
    {ok, RecvTO} = application:get_env(?APP, control_receive_timeout),
    RecvTO.

%% Merges user-defined send timeout values with app timeout values
-spec get_send_timeout(undefined | timeout()) -> timeout().
get_send_timeout(undefined) ->
    {ok, SendTO} = application:get_env(?APP, send_timeout),
    SendTO;
get_send_timeout(Else) ->
    Else.

%% Returns default inactivity timeouts for different modules
-spec get_inactivity_timeout(gen_rpc_client | gen_rpc_acceptor) -> timeout().
get_inactivity_timeout(gen_rpc_client) ->
    {ok, TTL} = application:get_env(?APP, client_inactivity_timeout),
    TTL;

get_inactivity_timeout(gen_rpc_acceptor) ->
    {ok, TTL} = application:get_env(?APP, server_inactivity_timeout),
    TTL.

-spec get_async_call_inactivity_timeout() -> timeout().
get_async_call_inactivity_timeout() ->
    {ok, TTL} = application:get_env(?APP, async_call_inactivity_timeout),
    TTL.

%%% ===================================================
%%% Private functions
%%% ===================================================
%% Returns the proper transport driver tuple
get_transport_driver_data_tuple(tcp) ->
    {tcp, gen_rpc_driver_tcp, tcp_closed, tcp_error};

get_transport_driver_data_tuple(ssl) ->
    {ssl, gen_rpc_driver_ssl, ssl_closed, ssl_error}.

hybrid_proplist_compare({K1,_V1}, {K2,_V2}) ->
    K1 =< K2;

hybrid_proplist_compare(K1, K2) ->
    K1 =< K2.