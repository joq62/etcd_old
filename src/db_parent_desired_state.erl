%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_parent_desired_state).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("db_parent_desired_state.hrl").

%% External exports
-export([create_table/0,create_table/2,add_node/2]).
-export([create/7,delete/1]).
-export([read_all/0,read/1,read/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([pods/1]).
-export([load_desired_state/1]).



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

create(ParentNode,NodeName,ClusterSpec,HostSpec,RootPaArgs,CommonFunsPaArgs,EnvArgs)->
    Record=#?RECORD{
		    parent_node=ParentNode,
		    node_name=NodeName,
		    cluster_spec=ClusterSpec,
		    host_spec=HostSpec,
		    root_pa_args=RootPaArgs,
		    common_funs_pa_args=CommonFunsPaArgs,
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

member(ParentNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.parent_node==ParentNode])),
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
pods(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
    Result=case Z of
	       []->
		  [];
	       _->
		   Pods=[R#?RECORD.parent_node||R<-Z],
		   Pods
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

read(Key,ParentNode)->
    Return=case read(ParentNode) of
	       []->
		   {error,[eexist,ParentNode,?MODULE,?LINE]};
	       {ParentNode,NodeName,ClusterSpec,HostSpec,RootPaArgs,CommonFunsPaArgs,EnvArgs} ->
		   case  Key of
		      parent_node->
			   {ok,ParentNode};
		       node_name->
			   {ok,NodeName};
		       cluster_spec->
			   {ok,ClusterSpec};
		       host_spec->
			   {ok,HostSpec};
		       root_pa_args->
			   {ok,RootPaArgs};
		       common_funs_pa_args->
			   {ok,CommonFunsPaArgs};
		       env_args->
			   {ok,EnvArgs};
		       Err ->
			   {error,['Key eexists',Err,ParentNode,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [R#?RECORD.parent_node||R<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{R#?RECORD.parent_node,R#?RECORD.node_name,R#?RECORD.cluster_spec,
      R#?RECORD.host_spec,R#?RECORD.root_pa_args,R#?RECORD.common_funs_pa_args,
      R#?RECORD.env_args}||R<-Z].

read(ParentNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.parent_node==ParentNode])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{R#?RECORD.parent_node,R#?RECORD.node_name,R#?RECORD.cluster_spec,
			   R#?RECORD.host_spec,R#?RECORD.root_pa_args,R#?RECORD.common_funs_pa_args,
			    R#?RECORD.env_args}||R<-Z],
		   Info
	   end,
    Result.

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
load_desired_state(ClusterSpec)->
    Result=case db_cluster_spec:read(pods,ClusterSpec) of
	       {ok,Pods}->	  
		   LoadResult=[{error,Reason}|| {error,Reason}<-load_desired_state(Pods,ClusterSpec,[])],
		   case LoadResult of
		       []->
			   ok;
		       ErrorList ->
			   {error,ErrorList}
		   end;
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ,db_cluster_spec,read,[pods,ClusterSpec: ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.
    
load_desired_state([],_ClusterSpec,Acc)->
    Acc;
load_desired_state([{_NumPods,HostSpec}|T],ClusterSpec,Acc) ->
    false=lists:member({ok,HostSpec},Acc),
    Result=case db_cluster_spec:read(root_dir,ClusterSpec) of
	       {ok,RootDir}->
		   case db_host_spec:read(hostname,HostSpec) of 
		       {ok,HostName}->
			   NodeName=ClusterSpec++"_parent",
			   ParentNode=list_to_atom(NodeName++"@"++HostName),
			   RootPaArgs=" -pa "++RootDir++" ",
			   PathCommonFuns=filename:join([RootDir,"*","ebin"]),
			   CommonFunsPaArgs=" -pa "++PathCommonFuns,
			   EnvArgs=" ",
			   case db_parent_desired_state:create(ParentNode,NodeName,ClusterSpec,HostSpec,
							       RootPaArgs,CommonFunsPaArgs,EnvArgs) of
			       {atomic,ok}->
				   ok;
			       Reason->
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ,db_host_spec,read,[hostname,HostSpec: ",Reason,?MODULE,?LINE]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ,db_cluster_spec,read,[root_dir,ClusterSpec: ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    load_desired_state(T,ClusterSpec,[Result|Acc]).
