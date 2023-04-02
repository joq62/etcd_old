-define(ProviderSpecDir,"provider_specs").
-define(GitPathProviderSpecs,"https://github.com/joq62/provider_specs.git").

-define(TABLE,provider_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec,
		 appl_name,
		 vsn,
		 dir,
		 node_name,
		 cookie,
		 tar_file,
		 clone_cmd,
		 tar_cmd,
		 start_cmd,
		 num,
		 affinity	
		}).


