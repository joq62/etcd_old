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
-module(all).      
    
 
-export([start/1

	]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start([_ClusterSpec,_HostSpec])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=setup(),
    ok=test_1(),
    ok=cluster_spec("cluster_test"),
    ok=host_spec(),
    ok=appl_spec(),
    ok=appl_deployment(),
    ok=parent_desired_state("cluster_test"),
    ok=pod_appl_desired_state("cluster_test"),
  %  ok=test_2(),
  %  ok=test_3(),
   
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    io:format(" init stop ~p~n",[init:stop()]),
    timer:sleep(2000),

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
cluster_spec(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    
    ok=cluster_spec_test:start(N1,ClusterSpec),
    ok=cluster_spec_test:start(N2,ClusterSpec),
    ok=cluster_spec_test:start(N3,ClusterSpec),
    ok=cluster_spec_test:start(N4,ClusterSpec),
    
    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
pod_appl_desired_state(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    
    ok=pod_desired_state_test:start(N1,ClusterSpec),
    ok=pod_desired_state_test:start(N2,ClusterSpec),
    ok=pod_desired_state_test:start(N3,ClusterSpec),
    ok=pod_desired_state_test:start(N4,ClusterSpec),
    
    ok.
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
parent_desired_state(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    
    ok=parent_desired_state_test:start(N1,ClusterSpec),
    ok=parent_desired_state_test:start(N2,ClusterSpec),
    ok=parent_desired_state_test:start(N3,ClusterSpec),
    ok=parent_desired_state_test:start(N4,ClusterSpec),
    
    ok.
    

    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
host_spec()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    ok=host_spec_tests:start(N1),
    io:format(" N1 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=host_spec_tests:start(N2),
    io:format(" N2 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=host_spec_tests:start(N3),
    io:format(" N3 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=host_spec_tests:start(N4),
    io:format(" N4 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% kill N3
    io:format("kill N3  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:call(N3,init,stop,[],5000),
    timer:sleep(1500),
    {badrpc,nodedown}=rpc:call(N3,db_host_spec,read,["c50"],5000),
    io:format("Kill N3  OK! ~p~n",[{?MODULE,?LINE}]),
  
    yes=rpc:call(N4,mnesia,system_info,[],5000),

    {ok,N3}=test_nodes:start_slave("c3"),
    [rpc:call(N3,net_adm,ping,[N],5000)||N<-AllNodes],
    true=rpc:call(N3,code,add_patha,["ebin"],5000),    
    true=rpc:call(N3,code,add_patha,["tests_ebin"],5000),     
    true=rpc:call(N3,code,add_patha,["common/ebin"],5000),     
    ok=rpc:call(N3,application,start,[common],5000), 
    true=rpc:call(N3,code,add_patha,["sd/ebin"],5000),     
    ok=rpc:call(N3,application,start,[sd],5000), 
    ok=rpc:call(N3,application,start,[etcd],5000),
    pong=rpc:call(N3,etcd,ping,[],5000),
    pong=rpc:call(N3,db,ping,[],5000),
       
    ok=host_spec_tests:start(N1),
    ok=host_spec_tests:start(N2),
    ok=host_spec_tests:start(N3),
    io:format("Restart N3 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=host_spec_tests:start(N4),

    ok.

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
appl_spec()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    ok=appl_spec_tests:start(N1),
    io:format(" N1 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_spec_tests:start(N2),
    io:format(" N2 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_spec_tests:start(N3),
    io:format(" N3 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_spec_tests:start(N4),
    io:format(" N4 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% kill N3
    io:format("kill N3  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:call(N3,init,stop,[],5000),
    timer:sleep(1500),
    {badrpc,nodedown}=rpc:call(N3,db_appl_spec,read,["c50"],5000),
    io:format("Kill N3  OK! ~p~n",[{?MODULE,?LINE}]),
  
    yes=rpc:call(N4,mnesia,system_info,[],5000),

    {ok,N3}=test_nodes:start_slave("c3"),
    [rpc:call(N3,net_adm,ping,[N],5000)||N<-AllNodes],
    true=rpc:call(N3,code,add_patha,["ebin"],5000),    
    true=rpc:call(N3,code,add_patha,["tests_ebin"],5000),     
    true=rpc:call(N3,code,add_patha,["common/ebin"],5000),     
    ok=rpc:call(N3,application,start,[common],5000), 
    true=rpc:call(N3,code,add_patha,["sd/ebin"],5000),     
    ok=rpc:call(N3,application,start,[sd],5000), 
    ok=rpc:call(N3,application,start,[etcd],5000),
    pong=rpc:call(N3,etcd,ping,[],5000),
    pong=rpc:call(N3,db,ping,[],5000),
       
    ok=appl_spec_tests:start(N1),
    ok=appl_spec_tests:start(N2),
    ok=appl_spec_tests:start(N3),
    io:format("Restart N3 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_spec_tests:start(N4),

    ok.

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
appl_deployment()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    ok=appl_deployment_tests:start(N1),
    io:format(" N1 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_deployment_tests:start(N2),
    io:format(" N2 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_deployment_tests:start(N3),
    io:format(" N3 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_deployment_tests:start(N4),
    io:format(" N4 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    %% kill N3
    io:format("kill N3  ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    rpc:call(N3,init,stop,[],5000),
    timer:sleep(1500),
    {badrpc,nodedown}=rpc:call(N3,db_appl_deployment,read,["c50"],5000),
    io:format("Kill N3  OK! ~p~n",[{?MODULE,?LINE}]),
  
    yes=rpc:call(N4,mnesia,system_info,[],5000),

    {ok,N3}=test_nodes:start_slave("c3"),
    [rpc:call(N3,net_adm,ping,[N],5000)||N<-AllNodes],
    true=rpc:call(N3,code,add_patha,["ebin"],5000),    
    true=rpc:call(N3,code,add_patha,["tests_ebin"],5000),     
    true=rpc:call(N3,code,add_patha,["common/ebin"],5000),     
    ok=rpc:call(N3,application,start,[common],5000), 
    true=rpc:call(N3,code,add_patha,["sd/ebin"],5000),     
    ok=rpc:call(N3,application,start,[sd],5000), 
    ok=rpc:call(N3,application,start,[etcd],5000),
    pong=rpc:call(N3,etcd,ping,[],5000),
    pong=rpc:call(N3,db,ping,[],5000),
       
    ok=appl_deployment_tests:start(N1),
    ok=appl_deployment_tests:start(N2),
    ok=appl_deployment_tests:start(N3),
    io:format("Restart N3 OK! ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=appl_deployment_tests:start(N4),

    ok.

    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
test_1()->
  io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    AllNodes=test_nodes:get_nodes(),
    [N1,N2,N3,N4]=AllNodes,
    %% Init
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
      

    %% N1
    ok=rpc:call(N1,application,start,[etcd],5000),
    pong=rpc:call(N1,etcd,ping,[],5000),
    pong=rpc:call(N1,db,ping,[],5000),

    [N1]=lists:sort(rpc:call(N1,mnesia,system_info,[running_db_nodes],5000)),
    io:format("N1 dist OK! ~p~n",[{?MODULE,?LINE}]),
 %   yes=rpc:call(N1,mnesia,system_info,[],5000),
 
    %% N2
    ok=rpc:call(N2,application,start,[etcd],5000),
    pong=rpc:call(N2,etcd,ping,[],5000),
    pong=rpc:call(N2,db,ping,[],5000),
    [N1,N2]=lists:sort(rpc:call(N1,mnesia,system_info,[running_db_nodes],5000)),
    [N1,N2]=lists:sort(rpc:call(N2,mnesia,system_info,[running_db_nodes],5000)),
 %   yes=rpc:call(N2,mnesia,system_info,[],5000),
    io:format("N2 dist OK! ~p~n",[{?MODULE,?LINE}]),

  %% N3
    ok=rpc:call(N3,application,start,[etcd],5000),
    pong=rpc:call(N3,etcd,ping,[],5000),
    pong=rpc:call(N3,db,ping,[],5000),
  
    [N1,N2,N3]=lists:sort(rpc:call(N1,mnesia,system_info,[running_db_nodes],5000)),
    [N1,N2,N3]=lists:sort(rpc:call(N2,mnesia,system_info,[running_db_nodes],5000)),
    [N1,N2,N3]=lists:sort(rpc:call(N3,mnesia,system_info,[running_db_nodes],5000)),
 %   yes=rpc:call(N3,mnesia,system_info,[],5000),
    io:format("N3 dist OK! ~p~n",[{?MODULE,?LINE}]),
 %% N4
    ok=rpc:call(N4,application,start,[etcd],5000),
    pong=rpc:call(N4,etcd,ping,[],5000),
    pong=rpc:call(N4,db,ping,[],5000),
    [N1,N2,N3,N4]=lists:sort(rpc:call(N1,mnesia,system_info,[running_db_nodes],5000)),
    [N1,N2,N3,N4]=lists:sort(rpc:call(N2,mnesia,system_info,[running_db_nodes],5000)),
    [N1,N2,N3,N4]=lists:sort(rpc:call(N3,mnesia,system_info,[running_db_nodes],5000)),
    [N1,N2,N3,N4]=lists:sort(rpc:call(N4,mnesia,system_info,[running_db_nodes],5000)),
   
 %   yes=rpc:call(N4,mnesia,system_info,[],5000),
    io:format("N4 dist OK! ~p~n",[{?MODULE,?LINE}]),
    ok.

%% -------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=test_nodes:start_nodes(),
    [rpc:call(N,code,add_patha,["ebin"],5000)||N<-test_nodes:get_nodes()],    
    [rpc:call(N,code,add_patha,["tests_ebin"],5000)||N<-test_nodes:get_nodes()],     
    [rpc:call(N,code,add_patha,["common/ebin"],5000)||N<-test_nodes:get_nodes()],     
    [rpc:call(N,application,start,[common],5000)||N<-test_nodes:get_nodes()], 
    [rpc:call(N,code,add_patha,["sd/ebin"],5000)||N<-test_nodes:get_nodes()],     
    [rpc:call(N,application,start,[sd],5000)||N<-test_nodes:get_nodes()], 
    
    ok.
