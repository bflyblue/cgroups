Fetch a list of cgroups
-----------------------

Returns list of cgroups, and URIs to resource.

Currently the heirarchy is hardcoded (cpuset).

    % curl -i http://localhost:3000/cgroups
    
    HTTP/1.1 200 OK
    Content-Type: application/json
    
    [
      { 
        "resource": "/"
        "uri": "/cgroup/",
      },
      { 
        "resource":"/Alpha"
        "uri": "/cgroup/Alpha",
      },
      {
        "resource":"/Alpha/1"
        "uri": "/cgroup/Alpha/1",
      }
    ]

Get tasks in a cgroup
---------------------

Returns list of tasks in a cgroup.

    % curl -i http://localhost:3000/cgroup/Alpha
    
    HTTP/1.1 200 OK
    Content-Type: application/json
    
    {
      "cgrpTasks": [
        { "taskId": "2953" },
        ...
        ],
      "cgrpName": "/Alpha"
    }

Assign task to a cgroup
-----------------------

Assign a task to a cgroup. 

    % curl -i http://localhost:3000/cgroup/Alpha -X PUT \
         --data-binary '{"taskId":"2953"}' -HContent-Type:application/json
    
    HTTP/1.1 200 OK
    Content-Type: application/json
    
    { "status": "success" }
