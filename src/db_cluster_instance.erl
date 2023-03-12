%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_cluster_instance).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("db_cluster_instance.hrl").

%% External exports
-export([nodes/2,pod_based_host_spec/3]).
-export([create_table/0,create_table/2,add_node/2]).
-export([create/7,delete/1]).
-export([read_all/0,read/1,read/2,read/3,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
%% Special functions

nodes(Type,ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		  X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.type==Type])),
    [X#?RECORD.pod_node||X<-Z].

pod_based_host_spec(HostSpec,Type,ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.host_spec==HostSpec,
		     X#?RECORD.type==Type])),
    [X#?RECORD.pod_node||X<-Z].
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {type,bag}
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

create(ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status)->
    Record=#?RECORD{
		    cluster_spec=ClusterSpec,
		    type=Type,
		    pod_name=PodName,
		    pod_node=PodNode,
		    pod_dir=PodDir,
		    host_spec=HostSpec,
		    status=Status

		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

delete(Object) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Object})
		    
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

member(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
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

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=[{X#?RECORD.cluster_spec,X#?RECORD.type,X#?RECORD.pod_name,X#?RECORD.pod_node,X#?RECORD.pod_dir,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z],
 
    Result.

read(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
    [{X#?RECORD.cluster_spec,X#?RECORD.type,X#?RECORD.pod_name,X#?RECORD.pod_node,X#?RECORD.pod_dir,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z].
 
read(ClusterSpec,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.pod_node==PodNode])),
    
   
    Result=case Z of
	       []->
		   [];
	       [X]->
		   {X#?RECORD.cluster_spec,X#?RECORD.type,X#?RECORD.pod_name,X#?RECORD.pod_node,X#?RECORD.pod_dir,X#?RECORD.host_spec,X#?RECORD.status}
	   end,
    Result.

read(Key,ClusterSpec,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.pod_node==PodNode])),
    Return=case Z of
	       []->
		   [];
	       [X]->
		   case  Key of
		       cluster_spec->
			   {ok,X#?RECORD.cluster_spec};
		       type->
			  {ok,X#?RECORD.type};
		       pod_name->
			   {ok,X#?RECORD.pod_name};
		       pod_node->
			    {ok,X#?RECORD.pod_node};
		       pod_dir->
			    {ok,X#?RECORD.pod_dir};
		       host_spec->
			    {ok,X#?RECORD.host_spec};
		       status->
			   {ok,X#?RECORD.status};
		       Err ->
			   {error,['Key eexists',Err,ClusterSpec,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [ClusterSpec||{?RECORD,ClusterSpec,_ConnectNode,_PodName,_PodNode,_PodDir,_HostSpec,_Status}<-Z].
    
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
