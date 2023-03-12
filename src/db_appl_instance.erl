%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_appl_instance).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("db_appl_instance.hrl").

%% External exports

-export([get_pod_appl_specs/1]).
-export([create_table/0,create_table/2,add_node/2]).
-export([create/5,delete/3]).
-export([read_all/0,read/1,read/2,read/3,get_all_id/0]).
-export([member/1]).
-export([do/1]).
-export([]).



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

get_pod_appl_specs(ClusterInstance)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance])),
    [{X#?RECORD.appl_spec,X#?RECORD.pod_node}||X<-Z].
    
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

create(ClusterInstance,ApplSpec,PodNode,HostSpec,Status)->
    Record=#?RECORD{
		    cluster_instance=ClusterInstance,
		    appl_spec=ApplSpec,
		    pod_node=PodNode,
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

delete(ClusterInstance,ApplSpec,PodNode) ->
    F = fun() -> 
		Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
				 X#?RECORD.cluster_instance==ClusterInstance,
				 X#?RECORD.appl_spec==ApplSpec,
				 X#?RECORD.pod_node==PodNode])),
		case Z of
		    []->
			mnesia:abort({error,[eexists_record,ClusterInstance,PodNode,?MODULE,?LINE]});
		    [X]->
			mnesia:delete_object(?TABLE, X, write)    
		end
	end,
    mnesia:transaction(F).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

member(ClusterInstance)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance])),
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
    Result=[{X#?RECORD.cluster_instance,X#?RECORD.appl_spec,X#?RECORD.pod_node,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z],
 
    Result.

read(ClusterInstance)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance])),
    [{X#?RECORD.cluster_instance,X#?RECORD.appl_spec,X#?RECORD.pod_node,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z].
    


read(ClusterInstance,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance,
		     X#?RECORD.pod_node==PodNode])),
    [{X#?RECORD.cluster_instance,X#?RECORD.appl_spec,X#?RECORD.pod_node,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z].

   

read(Key,ClusterInstance,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance,
		     X#?RECORD.pod_node==PodNode])),
    Return=case Z of
	       []->
		   [];
	       Z->
		   case  Key of
		       cluster_instance->
			   [X|_]=Z,
			   {ok,X#?RECORD.cluster_instance };
		       appl_spec->
			   R=[X#?RECORD.appl_spec||X<-Z],
			   {ok,R};
		       pod_node->
			   PodNode;
		       host_spec->
			   [X|_]=Z,
			   {ok,X#?RECORD.host_spec};
		       status->
			   R=[X#?RECORD.status||X<-Z],
			   {ok,R};
		       Err ->
			   {error,['Key eexists',Err,ClusterInstance,PodNode,?MODULE,?LINE]}
		   end
	   end,
    Return.



get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [ClusterInstance||{?RECORD,ClusterInstance,_ApplSpec,_PodNode,_Status}<-Z].

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
