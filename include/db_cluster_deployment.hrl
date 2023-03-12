-ifdef(debug).
define(ClusterDeploymentDir,"cluster_deployments_test").
-define(GitPathClusterDeployments,"https://github.com/joq62/cluster_deployments_test.git").
-else.
-define(ClusterDeploymentDir,"cluster_deployments").
-define(GitPathClusterDeployments,"https://github.com/joq62/cluster_deployments.git").
-endif.


-define(TABLE,cluster_deployment).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec_id,
		 cookie,
		 dir,
		 num_controllers,
		 controller_host_specs,
		 num_workers,
		 worker_host_specs
		}).
