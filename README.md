# HRest #

###Start

1 -stack ghci

2 - main

###Insert
curl -i  -X POST -d '{"name":"super bot"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/configurations
curl "http://localhost:8080/configuration/1"

###Update
curl -i -X PUT -d '{"name":"super bot updated"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/configuration/1
curl "http://localhost:8080/configuration/1"

###Delete
curl -i -X DELETE -i -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/configuration/1
