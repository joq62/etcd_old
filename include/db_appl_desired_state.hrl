-define(TABLE,appl_desired_state).
-define(RECORD,?TABLE).

-record(?RECORD,{
		 app,
		 appl_spec,
		 pod_node,
		 app_env
		 
		}).


