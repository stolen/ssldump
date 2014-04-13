ssldump
=======

Stupid proxy to dump SSL traffic

Pre-requisites
---------

 - Erlang (get it at https://www.erlang-solutions.com/downloads/download-erlang-otp)
 - Wget (for downloading dependency)

Usage
---------

To start dumper, pass wrapper script three arguments for defining proxy:

```
./ssldump.sh <LocalPort> <RemoteHost> <RemotePort>
```

For example:

```
./ssldump.sh 8443 github.com 443
```

