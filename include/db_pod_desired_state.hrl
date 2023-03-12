-define(TABLE,pod_desired_state).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 pod_node,
		 node_name,
		 pod_dir,
		 parent_node,
		 appl_spec_list,
		 cluster_spec,
		 host_spec,
		 pa_args_list,
		 env_args
		 
		}).


