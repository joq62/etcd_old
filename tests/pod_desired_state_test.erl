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
-module(pod_desired_state_test).      
 
-export([start/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(Node,ClusterSpec)->
    io:format("Start ~p~n",[{Node,ClusterSpec,?MODULE,?FUNCTION_NAME}]),

    ok=setup(Node),
    ok=load_desired_state(Node,ClusterSpec),
   
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state(Node,_ClusterSpec)->
    

    [
     {'1_c201_pod@c201',"1_c201_pod","c201/1_c201_pod",
      'c201_parent@c201',["common","sd"],"c201","c201",[]," "},
     {'1_cluster_test_pod@c200',"1_cluster_test_pod","cluster_test/1_cluster_test_pod",
      'cluster_test_parent@c200',[],"cluster_test","c200",[]," "},
     {'1_cluster_test_pod@c201',"1_cluster_test_pod","cluster_test/1_cluster_test_pod",
      'cluster_test_parent@c201',[],"cluster_test","c201",[]," "},
     {'2_c201_pod@c201',"2_c201_pod","c201/2_c201_pod",
      'c201_parent@c201',["common","sd"],"c201","c201",[]," "},
     {'2_cluster_test_pod@c200',"2_cluster_test_pod","cluster_test/2_cluster_test_pod",
      'cluster_test_parent@c200',[],"cluster_test","c200",[]," "},
     {'2_cluster_test_pod@c201',"2_cluster_test_pod","cluster_test/2_cluster_test_pod",
      'cluster_test_parent@c201',[],"cluster_test","c201",[]," "},
     {'3_c201_pod@c201',"3_c201_pod","c201/3_c201_pod",
      'c201_parent@c201',["common","sd"],"c201","c201",[]," "},
     {'3_cluster_test_pod@c200',"3_cluster_test_pod","cluster_test/3_cluster_test_pod",
      'cluster_test_parent@c200',[],"cluster_test","c200",[]," "},
     {'3_cluster_test_pod@c201',"3_cluster_test_pod","cluster_test/3_cluster_test_pod",
      'cluster_test_parent@c201',[],"cluster_test","c201",[]," "}
    ]=lists:sort(rpc:call(Node,db_pod_desired_state,read_all,[],5000)),


    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


setup(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=rpc:call(Node,etcd,ping,[],5000),
   
    ok.
