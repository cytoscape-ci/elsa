ELSA: The ErLang Submit Agent
=====

[![Build Status](https://travis-ci.org/cytoscape-ci/elsa.svg)](https://travis-ci.org/cytoscape-ci/elsa) [![GitHub version](https://badge.fury.io/gh/cytoscape-ci%2Felsa.svg)](https://badge.fury.io/gh/cytoscape-ci%2Felsa)

This node connects clients and services running across a network, and provides a mechanism for handling long running services. Elsa nodes connect together to form a relay network, where client calls route to agents that have the required services to fufill their request. For services that take longer than the tolerated time of the client, responses enter a buffer that the agent handles, and await client pickup at a later time. Please see below for instructions on how to deploy an agent, make calls through the agent as a client, and register with an agent as a service.

Getting Started
-----

Elsa uses Docker Hub automated builds for production use: [Elsa on Docker Hub](https://hub.docker.com/r/cytoscape/elsa).

You will need a Docker daemon to deploy the agent. Once you have docker installed, run:

``` docker run -p 8080:8080 cytoscape/elsa #Runs elsa in a container on port 8080 ```

This will bring up the agent on port 8080. Services and clients can now use the agent.

Releases and Production Deployments
-----
Releases are built automatically using Docker Hub automated builds. Images follow a master and tag scheme:

```
docker run cytoscape/elsa:latest #Run the container built off of master's last commit. Same as cytoscape/elsa with no tag.
docker run cytoscape/elsa:tag #Where tag is a release tag such as cytoscape/elsa:v0.1.0 for the v0.1.0 version of elsa.
```

For maximum stabability in production deployments, use a version tag. For the latest features and bug fixs, use the latest tag. You can also build your own release of elsa using the primary Dockerfile:

```
docker build -t elsa .
```

Clients
-----

####Request

A client can reach a registered service by calling:

``` www.domain.com:port/service/version/endpoint ```

For example, with an agent connect to port 8080 on an instance called elsa, and trying to reach version one of the service test's status endpoint the call would look like this:

``` curl http://elsa:8080/test/v1/status ```

####Response

A client may include an optional *X-ELSA-TIMEOUT* header in the request. If the service responds within a number of milliseconds specified in this header, the client will receive its data as it normally would as though it had called the service directly. This field defaults to 5 seconds.

If the service misses the timeout window, the agent sends back a 300 status code and a url that the client may check periodically to retrieve its data. When it quieries this url, it will receive one of the following:

```
    404 - Task not found. #The task either failed to complete, or the task does not exist.
    204 - Task not yet completed. Please try again later. #The agent still awaits data from the service. Retry later.
    200 #Your task completed successfully, this response contains the response of the service.
```

Services
-----
A service must send a JSON document called it's registration to an agent in order to receive forwarded calls from clients. When a service shuts down, it should then send its registration to the agent with a DELETE request:

```
POST elsa:8080/registration #Registers the instance if its registration document is valid.
DELETE elsa:8080/registration #Unregisters the instance if its registration document is valid.

```

For the format of a registration document, please reference the  [registration](https://github.com/cytoscape-ci/elsa/blob/master/REGISTRATION.md) spec.


Development
------
You can develop through either docker or by running the agent on an Erlang virtual machine.

####Erlang Virtual Machine Development
First install the latest version of the Erlang virtual machine. Clone the repo, and then use the rebar3 script to
develop with the agent:

```
git clone https://github.com/cytoscape-ci/elsa.git && cd elsa

    ./rebar3 shell   #Run elsa in an interactive shell.
    ./rebar3 eunit   #Run the eunit test framework
    ./rebar3 release #Create a release of elsa

```

####Docker Container Development

To develop in a container, you should build your changes with the devleopment Dockerfile:

```
docker build -f Dockerfile.develop -t elsa .
```

You can then run any rebar3 command by appending it to docker run like so:

```
docker run -it elsa shell #Equivelant to ./rebar3 shell
```

Contributing
------
Contributions are welcome in the form of issues or pull requests. For code changes, please submit unit tests in the tests directory. These tests may either use Common Test or Eunit.

Licensing
=========
You can find the LGPL License for elsa [here](https://github.com/cytoscape-ci/elsa/blob/master/LICENSE.txt).
