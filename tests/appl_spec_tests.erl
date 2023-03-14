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
    
    true=lists:member("db_test",db_appl_spec:get_all_id()),
    
    {"db_test",
     "db_test",
     "0.1.0",
     math,
     "https://github.com/joq62/math.git"
    }=db_appl_spec:read("db_test"),
    
    {ok,"db_test"}=db_appl_spec:read(appl_name,"db_test"),
    {ok,"0.1.0"}=db_appl_spec:read(vsn,"db_test"),
    {ok,math}=db_appl_spec:read(app,"db_test"),
    {ok,"https://github.com/joq62/math.git"}=db_appl_spec:read(gitpath,"db_test"),
    {error,['Key eexists',glurk,"db_test",db_appl_spec,_]}=db_appl_spec:read(glurk,"db_test"),
    {error,[eexist,"glurk",db_appl_spec,_]}=db_appl_spec:read( vsn,"glurk"),

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


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=db_etcd:ping(),
    
    ok.
