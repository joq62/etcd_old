-define(TABLE,parent_desired_state).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 parent_node,
		 node_name,
		 cluster_spec,
		 host_spec,
		 root_pa_args,
		 common_funs_pa_args,
		 env_args
		 
		}).


