%%% @author c50 <joq62@c50>
%%% @copyright (C) 2022, c50
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2022 by c50 <joq62@c50>

-module(db_config).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%-import(lists, [foreach/2]).
-include_lib("stdlib/include/qlc.hrl").
-include("db_config.hrl").


%% External exports
-export([create_table/0,create_table/2,add_node/2]).
-export([set/2,get/1,get_all/0,delete/1]).
-export([]).
-export([do/1]).
-export([]).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {type,set}
				
				]),
    mnesia:wait_for_tables([?TABLE], 2*20000).

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

set(Key,Value)->
   
    F = fun() ->
		Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
				 X#?RECORD.key==Key])),
		case Z of
		    [] ->
			NewRecord=#?RECORD{key=Key,value=Value},
			mnesia:write(NewRecord);
		    [CurrentRecord]->
			mnesia:delete({?TABLE,CurrentRecord}),
			NewRecord=#?RECORD{key=Key,value=Value},
			mnesia:write(NewRecord)
		end
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

get(Key)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.key==Key])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Value]=[Record#?RECORD.value||Record<-Z],
		   Value
	   end,
    Result.
    

get_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Record#?RECORD.key,Record#?RECORD.value}||Record<-Z].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

delete(Key) ->
    F = fun() ->
		Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
				 X#?RECORD.key==Key])),
		case Z of
		    [] ->
			mnesia:abort({error,["Key not exists ",Key]});
		    _->
			mnesia:delete({?TABLE,Key})
		end
	end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


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

