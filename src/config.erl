%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(config).    
     
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).

-export([
	start/0
	]).
%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start()->
    ok=db_cluster_spec:create_table(),
    ClusterSpecList=db_cluster_spec:git_clone_load(),
    Ok_ClusterSpec=[X||{ok,X}<-ClusterSpecList],
    Err_ClusterSpec=[X||{error,X}<-ClusterSpecList],
  
    ok=db_host_spec:create_table(),
    HostSpecList=db_host_spec:git_clone_load(),
    Ok_HostSpec=[X||{ok,X}<-HostSpecList],
    Err_HostSpec=[X||{error,X}<-HostSpecList],

    ok=db_cluster_instance:create_table(),

    ok=db_appl_spec:create_table(),
    ApplSpecList=db_appl_spec:git_clone_load(),
    Ok_ApplSpec=[X||{ok,X}<-ApplSpecList],
    Err_ApplSpec=[X||{error,X}<-ApplSpecList],

    ok=db_appl_deployment:create_table(),
    ApplDeploymentList=db_appl_deployment:git_clone_load(),
    Ok_ApplDeployment=[X||{ok,X}<-ApplDeploymentList],
    Err_ApplDeployment=[X||{error,X}<-ApplDeploymentList],

    ok=db_parent_desired_state:create_table(),
    ok=db_pod_desired_state:create_table(),
    % to be removed
    %ok=db_appl_instance:create_table(),
   % ok=db_config:create_table(),
   
    Test=lists:append([Ok_ClusterSpec,Ok_HostSpec,Ok_ApplSpec,Ok_ApplDeployment,
		       Err_ClusterSpec,Err_HostSpec,Err_ApplSpec,Err_ApplDeployment]),
		       

    Result=case Test of
	       []->
		   {error,[{cluster,spec,Ok_ClusterSpec,Err_ClusterSpec},
			   {host_spec,Ok_HostSpec,Err_HostSpec},
			   {appl_spec,Ok_ApplSpec,Err_ApplSpec},
			   {appl_deployment,Ok_ApplDeployment,Err_ApplDeployment}]};
	       _ ->
		   ok
	   end,
    Result.
