%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(cluster_spec_tests).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=read_specs_test(),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ClusterSpec="c200_c201",
    true=lists:member(ClusterSpec,db_cluster_spec:get_all_id()),

    {"c200_c201","cookie_c200_c201","c200_c201",[{6,"c200"},{6,"c201"}]}=db_cluster_spec:read(ClusterSpec),
    
    {ok,ClusterSpec}=db_cluster_spec:read(cluster_spec,ClusterSpec),
    {ok,"cookie_c200_c201"}=db_cluster_spec:read(cookie,ClusterSpec),
    {ok,ClusterSpec}=db_cluster_spec:read(root_dir,ClusterSpec),
    {ok,[{6,"c200"},{6,"c201"}]}=db_cluster_spec:read(pods,ClusterSpec),
   
    {error,[eexist,"glurk",db_cluster_spec,_]}=db_cluster_spec:read(cookie,"glurk"),
    {error,['Key eexists',glurk,"c200_c201",db_cluster_spec,_]}=db_cluster_spec:read(glurk,ClusterSpec),
 
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=db_etcd:ping(),
    
    ok.
