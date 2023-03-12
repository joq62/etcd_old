%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(etcd).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(SERVER,etcd_server).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	
	 ping/0

	]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

is_config()->
 %   io:format("DEBUG  ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    gen_server:call(?SERVER, {is_config}).
config()->
 %   io:format("DEBUG  ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    gen_server:call(?SERVER, {config}).
    
ping() ->
    gen_server:call(?SERVER, {ping}).




%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
