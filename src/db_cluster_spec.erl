%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_cluster_spec).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("db_cluster_spec.hrl").

%% External exports
-export([create_table/0,create_table/2,add_node/2]).
-export([create/4,delete/1]).
-export([read_all/0,read/1,read/2,get_all_id/0]).
-export([do/1]).
-export([member/1]).
-export([git_clone_load/0]).


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

create(ClusterSpec,Cookie,RootDir,Pods)->
    Record=#?RECORD{
		    cluster_spec=ClusterSpec,
		    cookie=Cookie,
		    root_dir=RootDir,
		    pods=Pods
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
delete(ClusterSpec) ->
    F = fun() ->
                mnesia:delete({?TABLE,ClusterSpec})

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

read(Key,ClusterSpec)->
    Return=case read(ClusterSpec) of
	       []->
		   {error,[eexist,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,Cookie,RootDir,Pods} ->
		   case  Key of
		      cluster_spec->
			   {ok,ClusterSpec};
		       cookie->
			   {ok,Cookie};
		       root_dir->
			   {ok,RootDir};
		       pods->
			   {ok,Pods};
		       Err ->
			   {error,['Key eexists',Err,ClusterSpec,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [Record#?RECORD.cluster_spec||Record<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Record#?RECORD.cluster_spec,Record#?RECORD.cookie,
      Record#?RECORD.root_dir,Record#?RECORD.pods}||Record<-Z].

read(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{Record#?RECORD.cluster_spec,Record#?RECORD.cookie,
			    Record#?RECORD.root_dir,Record#?RECORD.pods}||Record<-Z],
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

git_clone_load()->
    ok=create_table(),
    Result=case git_clone() of
	       {error,Reason}->
		   {error,Reason};
	       {ok,TempDirName,SpecDir}->
		   case from_file(SpecDir) of
		       {error,Reason}->
			   file:del_dir_r(TempDirName),	
			   {error,Reason};
		       LoadResult->
			   file:del_dir_r(TempDirName),		
			   LoadResult
		   end
	   end,
    Result.

git_clone()->
    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
    ok=file:make_dir(TempDirName),
    true=filelib:is_dir(TempDirName),

    GitDir=filename:join(TempDirName,?ClusterSpecDir),
    ok=file:make_dir(GitDir),
    GitPath=?GitPathClusterSpecs,
    {ok,GitResult}=cmn_appl:git_clone_to_dir(node(),GitPath,GitDir),
     Result=case filelib:is_dir(GitDir) of
	       false->
		   {error,[failed_to_clone,GitPath,GitResult]};
	       true->
		   {ok,TempDirName,GitDir}
	   end,
    Result.	

from_file(Dir)->
    {ok,FileNames}=file:list_dir(Dir),
    from_file(FileNames,Dir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    FullFileName=filename:join(Dir,FileName),
    NewAcc=case file:consult(FullFileName) of
	       {error,Reason}->
		   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
	       {ok,[{cluster_spec,ClusterSpec,Info}]}->
		   {cookie,Cookie}=lists:keyfind(cookie,1,Info),
		   {root_dir,RootDir}=lists:keyfind(root_dir,1,Info),
		   {pods,Pods}=lists:keyfind(pods,1,Info),
		   case create(ClusterSpec,Cookie,RootDir,Pods) of
		       {atomic,ok}->
			   [{ok,FileName}|Acc];
		       {error,Reason}->
			   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
		   end;
	       {ok,NotAnApplSpecFile} -> 
		   [{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
 %   io:format("NewAcc ~p~n",[{NewAcc,?MODULE,?LINE}]),
    from_file(T,Dir,NewAcc).
	
  
