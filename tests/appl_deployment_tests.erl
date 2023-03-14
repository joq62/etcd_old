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
-module(appl_deployment_tests).      
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(Node),
    ok=read_specs_test(Node),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    All=lists:sort(rpc:call(Node,db_appl_deployment,get_all_id,[],5000)),
    true=lists:member("db_test",All),

   {"db_test","db_dummy_application",
    "0.1.0","prototype_c201",1,["c201"]}=rpc:call(Node,db_appl_deployment,read,["db_test"],5000),
    
    {ok,"db_dummy_application"}=rpc:call(Node,db_appl_deployment,read,[appl_spec,"db_test"],5000),
    {ok,"0.1.0"}=rpc:call(Node,db_appl_deployment,read,[vsn,"db_test"],5000),
    {ok,1}=rpc:call(Node,db_appl_deployment,read,[num_instances,"db_test"],5000),
    {ok,["c201"]}=rpc:call(Node,db_appl_deployment,read,[affinity,"db_test"],5000),
   
    {error,[eexist,"glurk",db_appl_deployment,_]}=rpc:call(Node,db_appl_deployment,read,[vsn,"glurk"],5000),
    {error,['Key eexists',glurk,"db_test",db_appl_deployment,_]}=rpc:call(Node,db_appl_deployment,read,[glurk,"db_test"],5000),
 
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


setup(Node)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 
    pong=rpc:call(Node,etcd,ping,[],5000),
   
    ok.
