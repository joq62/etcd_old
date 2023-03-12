-ifdef(debug).
-define(ClusterSpecDir,"cluster_specs_test").
-define(GitPathClusterSpecs,"https://github.com/joq62/cluster_specs_test.git").
-else.
-define(ClusterSpecDir,"cluster_specs").
-define(GitPathClusterSpecs,"https://github.com/joq62/cluster_specs.git").
-endif.

-define(TABLE,cluster_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 cluster_spec,
		 cookie,
		 root_dir,
		 pods
		}).
