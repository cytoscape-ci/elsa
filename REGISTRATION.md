Registration
=====

There are three primary fields:

- service: The name of the service as a string
- version: The version of the service API as a string, should be of the form ("v1", "v2", "vN"...)
- instances: An list containing instance objects

An instance object has two fields
- location: This should be a domain name or ip address. http:// will be appended if
the protocol is not specified. This will also make the default port 80.
- capacity: An integer or the string "infinity", this tells the agent how many concurrent
requests your service can handle. Infinity means that your service can handle an unlimited
number of concurrent connections.

This is a minimum registration document.
-----

```json
{
  "service": "service1",
  "version": "v1",
  "instances": [
    {
      "location": "https://1.1.1.1:8080",
      "capacity": "infinity"
    }
  ]
}
```

One service with multiple instances.
-----

```json
{
  "service": "service1",
  "version": "v1",
  "instances": [
    {
      "capacity": "1.1.1.1",
      "capacity": "infinity"
    },
    {
      "location": "1.1.1.2",
      "capacity": 44
    }
  ]
}
```

Multiple service versions (or services) with multiple instances.
----

```json
[
  {
    "service": "service1",
    "version": "v1",
    "instances": [
      {
        "location": "1.1.1.1:3000",
        "capacity": "infinity"
      },
      {
        "location": "1.1.1.2",
        "capacity": 44
      }
    ]
  },
  {
    "service": "service2",
    "version": "v1",
    "instances": [
      {
        "location": "1.1.1.6",
        "capacity": "infinity"
      },
      {
        "location": "1.1.1.9:80",
        "capacity": 44
      }
    ]
  }
]
```
