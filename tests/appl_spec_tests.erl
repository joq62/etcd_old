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
-module(appl_spec_tests).      
 
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
    
    true=lists:member("db_test",rpc:call(Node,db_appl_spec,get_all_id,[],5000)),
    
    {"db_test","db_dummy_application","1.2.3",db_dummy,
     "https://github.com/joq62/db_dummy_git_path.git"}=rpc:call(Node,db_appl_spec,read,["db_test"],5000),
    
    {ok,"db_dummy_application"}=rpc:call(Node,db_appl_spec,read,[appl_name,"db_test"],5000),
    {ok,"1.2.3"}=rpc:call(Node,db_appl_spec,read,[vsn,"db_test"],5000),
    {ok,db_dummy}=rpc:call(Node,db_appl_spec,read,[app,"db_test"],5000),
    {ok,"https://github.com/joq62/db_dummy_git_path.git"}=rpc:call(Node,db_appl_spec,read,[gitpath,"db_test"],5000),
    {error,['Key eexists',glurk,"db_test",db_appl_spec,_]}=rpc:call(Node,db_appl_spec,read,[glurk,"db_test"],5000),
    {error,[eexist,"glurk",db_appl_spec,_]}=rpc:call(Node,db_appl_spec,read,[vsn,"glurk"],5000),

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
