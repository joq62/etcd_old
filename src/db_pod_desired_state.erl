%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_pod_desired_state).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("db_pod_desired_state.hrl").

%% External exports
-export([create_table/0,create_table/2,add_node/2]).
-export([create/9,delete/1]).
-export([read_all/0,read/1,read/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([pods/1]).
-export([add_appl_list/2,delete_appl_list/2,
	 get_pods_based_app/1]).

-export([load_desired_state_pod/1,
	 load_desired_state_appl/1]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}
				]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create(PodNode,NodeName,PodDir,ParentNode,ApplSpecList,ClusterSpec,HostSpec,PaArgsList,EnvArgs)->
    Record=#?RECORD{
		    pod_node=PodNode,
		    node_name=NodeName,
		    pod_dir=PodDir,
		    parent_node=ParentNode,
		    appl_spec_list=ApplSpecList,
		    cluster_spec=ClusterSpec,
		    host_spec=HostSpec,
		    pa_args_list=PaArgsList,
		    env_args=EnvArgs		   
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete(ParentNode) ->
    F = fun() ->
                mnesia:delete({?TABLE,ParentNode})
        end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

member(PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.pod_node==PodNode])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
% ApplList {PodNode,ApplSpec,App}
%get_pods_based_appl_spec(ApplSpec)->
%    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
%    PodsApp=[R#?RECORD.pod_node||R<-Z,
%				 ApplSpec=:=R#?RECORD.appl_spec_list],
%    {ok,PodsApp}.
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
pods(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
    Result=case Z of
	       []->
		  [];
	       _->
		   Pods=[R#?RECORD.pod_node||R<-Z],
		   Pods
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_pods_based_app(App)->
    F = fun() ->
                Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
                case Z of
                    [] ->
			mnesia:abort({error,["empty table "]});
		    Z->
			PodsApplSpecs=[{R#?RECORD.pod_node,R#?RECORD.appl_spec_list}||R<-Z],
			check(PodsApplSpecs,App,[])
                end
        end,
    mnesia:transaction(F).  


    
check([],_,Pods)->
    Pods;
check([{Pod,ApplSpecList}|T],WantedApp,Acc) ->
    L=[Pod||AppSpec<-ApplSpecList,
	    {ok,WantedApp}==sd:call(db_etcd,db_appl_spec,read,[app,AppSpec],5000)],
    check(T,WantedApp,lists:append(L,Acc)).     

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

read(Key,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.pod_node==PodNode])),
    Result=case Z of
	       []->
		   {error,["PodNode doesnt exists",PodNode,?MODULE,?LINE]};
	      [Record]->
		   
    		   case  Key of
		       pod_node->
			   {ok,Record#?RECORD.pod_node};
		       node_name->
			   {ok,Record#?RECORD.node_name};
		       pod_dir->
			   {ok,Record#?RECORD.pod_dir};
		       parent_node->
			   {ok,Record#?RECORD.parent_node};
		       appl_spec_list->
			   {ok,Record#?RECORD.appl_spec_list};
		       cluster_spec->
			   {ok,Record#?RECORD.cluster_spec};		       
		       host_spec->
			   {ok,Record#?RECORD.host_spec};
		       pa_args_list->
			   {ok,Record#?RECORD.pa_args_list};
		       env_args->
			   {ok,Record#?RECORD.env_args};
		       Err ->
			   {error,['Key eexists',Err,PodNode,?MODULE,?LINE]}
		   end
	   end,
    Result.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.pod_node||R<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.pod_node,R#?RECORD.node_name,R#?RECORD.pod_dir,R#?RECORD.parent_node,
      R#?RECORD.appl_spec_list,R#?RECORD.cluster_spec,R#?RECORD.host_spec,R#?RECORD.pa_args_list,R#?RECORD.env_args}||R<-Z].

read(PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.pod_node==PodNode])),
    Result=case Z of
	       []->
		  [];
	       [R]->
		{R#?RECORD.pod_node,R#?RECORD.node_name,R#?RECORD.pod_dir,R#?RECORD.parent_node,
		 R#?RECORD.appl_spec_list,R#?RECORD.cluster_spec,R#?RECORD.host_spec,R#?RECORD.pa_args_list,R#?RECORD.env_args}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
add_appl_list(NewApplSpec,PodNode)->
    F = fun() ->
                Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
                                 X#?RECORD.pod_node==PodNode])),
                case Z of
                    [] ->
			mnesia:abort({error,["PodNode not exists ",PodNode]});
		    [R]->
			case lists:member(NewApplSpec,R#?RECORD.appl_spec_list) of
			    true->
				mnesia:abort({error,["ApplSpec already added to PodNode  ",NewApplSpec,PodNode]});
			    false->
				NewRecord=R#?RECORD{appl_spec_list=[NewApplSpec|lists:delete(NewApplSpec,R#?RECORD.appl_spec_list)]},
				mnesia:write(NewRecord)
			end
                end
        end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete_appl_list(DeleteApplSpec,PodNode)->
    F = fun() ->
                Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
                                 X#?RECORD.pod_node==PodNode])),
                case Z of
                    [] ->
			mnesia:abort({error,["ERROR: PodNode not exists ",PodNode]});
		    [R]->
			case lists:member(DeleteApplSpec,R#?RECORD.appl_spec_list) of
			    false->
				mnesia:abort({error,["ERROR: ApplSpec already removed to PodNode  ",DeleteApplSpec,PodNode]});
			    true->
				NewRecord=R#?RECORD{appl_spec_list=lists:delete(DeleteApplSpec,R#?RECORD.appl_spec_list)},
				mnesia:write(NewRecord)
			end
                end
        end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_candidate_pods(SpecId,HostSpec,ClusterSpec)->
    {ok,ApplSpec}=db_appl_deployment:read(appl_spec,SpecId),
    RightHost=[PodNode||PodNode<-db_pod_desired_state:get_all_id(),
			{ok,HostSpec}==db_pod_desired_state:read(host_spec,PodNode)],
    NodeApplSpecList=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==db_pod_desired_state:read(cluster_spec,PodNode)],
    prioritize(Candidates,[]).

prioritize([],Acc)->
    [PodNode||{_NumApplSpecs,PodNode}<-lists:keysort(1,Acc)];
prioritize([PodNode|T],Acc) ->
    {ok,ApplSpecList}=db_pod_desired_state:read(appl_spec_list,PodNode),
    NumApplSpecs=list_length:start(ApplSpecList),
    prioritize(T,[{NumApplSpecs,PodNode}|Acc]).
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state_pod(ClusterSpec)->
  %  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["DBG  : ",node(),?MODULE,?FUNCTION_NAME,?LINE]]),
    {ok,Pods}=db_cluster_spec:read(pods,ClusterSpec),
    LoadResult=[{error,Reason}|| {error,Reason}<-load_desired_state_pod(Pods,ClusterSpec,[])],
    case LoadResult of
	[]->
	    ok;
	ErrorList ->
	    {error,ErrorList}
    end.
    
load_desired_state_pod([],_ClusterSpec,Acc)->
    Acc;
load_desired_state_pod([{NumPods,HostSpec}|T],ClusterSpec,Acc) ->
    false=lists:member({ok,HostSpec},Acc),
    [ParentNode]=[ParentNode||ParentNode<-db_parent_desired_state:get_all_id(),
			      {ok,HostSpec}==db_parent_desired_state:read(host_spec,ParentNode),
			      {ok,ClusterSpec}==db_parent_desired_state:read(cluster_spec,ParentNode)],

    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
%    {ok,HostName}=db_host_spec:read,[hostname,HostSpec),
    BaseNodeName=ClusterSpec++"_pod",
    Result=load_info(NumPods,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,[]),
    load_desired_state_pod(T,ClusterSpec,[Result|Acc]).

load_info(0,_RootDir,_BaseNodeName,_ClusterSpec,_HostSpec,_ParentNode,Acc)->
    Acc;
load_info(N,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,Acc)->
    NodeName=integer_to_list(N)++"_"++BaseNodeName,
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    PodNode=list_to_atom(NodeName++"@"++HostName),
    PodDir=filename:join(RootDir,NodeName),
    PaArgsList=[],
    EnvArgs=" ",             
    AppSpecList=[],
    Result=case db_pod_desired_state:create(PodNode,NodeName,PodDir,ParentNode,AppSpecList,ClusterSpec,HostSpec,PaArgsList,EnvArgs) of
	       {atomic,ok}->
		   ok;
	       Reason->
		   {error,Reason}
	   end,
    load_info(N-1,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,[Result|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state_appl(ClusterSpec)->
    {ok,PodsHostList}=db_cluster_spec:read(pods,ClusterSpec),
    HostSpecList=[HostSpec||{_Num,HostSpec}<-PodsHostList],
    AllDeploymentId=db_appl_deployment:get_all_id(),
    ApplDeploymentSpecInfoList=[db_appl_deployment:read(ApplDeploymentId)||ApplDeploymentId<-AllDeploymentId,
			       {ok,ClusterSpec}==db_appl_deployment:read(cluster_spec,ApplDeploymentId)],
  %  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ApplDeploymentSpecInfoList : ",ApplDeploymentSpecInfoList,?MODULE,?LINE]]),
    _Result=main_load_desired_state_appl(ApplDeploymentSpecInfoList,HostSpecList,[]),
  %  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Result : ",Result,?MODULE,?LINE]]),
    ok.

main_load_desired_state_appl([],_HostSpecList,Acc)->
    Acc;
main_load_desired_state_appl([ApplDeploymentSpec|T],HostSpecList,Acc) ->
    Result=load_desired_state_appl(ApplDeploymentSpec,HostSpecList,[]),
    main_load_desired_state_appl(T,HostSpecList,[Result|Acc]).

%%-- Affinity on each pod each_pod
load_desired_state_appl({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,_,each_pod},[],Acc)->
    Acc;
load_desired_state_appl({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},[HostSpec|T],Acc)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ApplSpec : ",ApplSpec,?MODULE,?LINE]]),
    RightHost=[PodNode||PodNode<-db_pod_desired_state:get_all_id(),
			{ok,HostSpec}==db_pod_desired_state:read(host_spec,PodNode)],
    NodeApplSpecList=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==db_pod_desired_state:read(cluster_spec,PodNode)],
    AddResult=[{db_pod_desired_state:add_appl_list(ApplSpec,PodNode),ApplSpec,PodNode,HostSpec}||PodNode<-Candidates],
    ErrorResult=[{error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,X_ApplSpec,PodNode,X_HostSpec]}||
		    {{aborted,Reason},X_ApplSpec,PodNode,X_HostSpec}<-AddResult],
    OkResult=[{ok,X_ApplSpec,PodNode,X_HostSpec}||{{atomic,ok},X_ApplSpec,PodNode,X_HostSpec}<-AddResult],
    NewAcc=lists:append([OkResult,ErrorResult,Acc]),
    load_desired_state_appl({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},T,NewAcc);


%%-- Affinity any_host    
load_desired_state_appl({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,any_host},_HostList,Acc)->
    Acc;
load_desired_state_appl({SpecId,ApplSpec,_Vsn,ClusterSpec,N,any_host},[HostSpec|T],Acc)->
  %  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ApplSpec : ",ApplSpec,?MODULE,?LINE]]),
  %  io:format("ApplSpec,WantedHostSpec ~p~n",[{ApplSpec,HostSpec,?MODULE,?FUNCTION_NAME}]),
    Result=case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
	       []->
		   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
	       [PodNode|_]->
		   case db_pod_desired_state:add_appl_list(ApplSpec,PodNode) of
		       {aborted,Reason}->
			   {error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]};
		       {atomic,ok}->
			%   io:format("Ok  ~p~n",[{ApplSpec,PodNode,?MODULE,?FUNCTION_NAME}]),
			   ok
		   end
	   end,
    RotatedHostList=lists:append(T,[HostSpec]),
    load_desired_state_appl({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,any_host},RotatedHostList,[Result|Acc]);
		       
load_desired_state_appl({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,_Affinity},_HostList,Acc)->
    Acc;
load_desired_state_appl({SpecId,ApplSpec,_Vsn,ClusterSpec,N,[HostSpec|T]},HostList,Acc)->
  %  io:format("ApplSpec,WantedHostSpec ~p~n",[{ApplSpec,WantedHostSpec,?MODULE,?FUNCTION_NAME}]),
    Result=case lists:member(HostSpec,HostList) of
	       false->
		   {error,["ERROR: Host not part of cluster hosts : ",HostSpec,HostList]};
	       true->
		   case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
		       []->
			   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
		       [PodNode|_]->				   
			   case db_pod_desired_state:add_appl_list(ApplSpec,PodNode) of
			       {aborted,Reason}->
				   {error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]};
			       {atomic,ok}->
			%	   io:format("Ok  ~p~n",[{ApplSpec,PodNode,?MODULE,?FUNCTION_NAME}]),
				   ok
			   end
		   end
	   end,
    RotatedWantedHostList=lists:append(T,[HostSpec]),
    load_desired_state_appl({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,RotatedWantedHostList},HostList,[Result|Acc]).
