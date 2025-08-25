# Postgres Network Simulation Example
This repository demonstrates simulating intermittent network links (e.g., packet loss)
between Docker services using Docker Compose.


It includes a Postgres primary-replica cluster, an HAProxy proxy, and three clients,
with iproute2 pre-installed in a custom Postgres image for clean tc usage.


## Prerequisites

- Ubuntu 24.04
- Docker 28.3.3
- Docker Compose v2.32.4
- GNU Make

## Setup

Clone the repo: git clone <repo-url>

- Ensure no local Postgres instance is running on ports 5433 or 5434:
  - sudo systemctl stop postgresql or pkill -u postgres.

- Use the Makefile for common tasks:
  - Build and start services: make all
  - Start services (if image is built): make start
  - Stop services: make stop
  - Stop and remove resources: make cleanup
  - Full reset (including images): make clean
  - View container logs: make logs
  - Clean old logs: make clean-logs

Alternatively, manually:

- Build the custom Postgres image: docker build -t postgres-iproute2:16 .
- Start services: docker-compose up -d --remove-orphans
- Wait ~1-2 minutes for replication setup (check logs with docker logs db-secondary for "streaming WAL" to confirm).


## Password

See [docker-compose.yaml](docker-compose.yaml):
- POSTGRES_USER: user
- POSTGRES_DB: postgres
- POSTGRES_PASSWORD: password


## Scenarios

Scenario 1: ./scripts/scenario1.sh (20% packet loss from primary DB to proxy)
Scenario 2: ./scripts/scenario2.sh (20% packet loss from secondary DB to proxy)
Scenario 3: ./scripts/scenario3.sh (20% packet loss from client1 to proxy; edit for client2/client3)


### Test queries from a client (e.g., client1):

```bash
$ docker exec -it client1 bash
root@bcbf1fda6936:/# psql -h proxy -p 5432 -U user -d postgres -W -c 'SELECT 1;'
Password: 
 ?column? 
----------
        1
(1 row)
```

```bash
root@bcbf1fda6936:/# psql -h proxy -p 5432 -U user -d postgres -W -c 'SELECT * from USERS'
Password: 
 id |  name  |       email        
----+--------+--------------------
  1 | Anton  | anton.aus@tirol.de
  2 | Brenda | b.houston@usa.com
(2 rows)
```

### Test replication status

Check replication status on db-primary:

```bash
docker exec db-primary psql -U user -d postgres -p 5433 -c "SELECT * FROM pg_stat_replication;"
```

Check replication - create a table on db-primary:

```bash
docker exec db-primary psql -U user -d postgres -p 5433 -c "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT, email TEXT);"
```
Insert some test data:

```bash
docker exec db-primary psql -U user -d postgres -p 5433 -c "INSERT INTO users(name,email) VALUES ('Anton','anton.aus@tirol.de');"
```

Read the data back on db-secondary:

```bash
docker exec db-secondary psql -U user -d postgres -p 5433 -c "SELECT * from users;"
```

Test if locally installed **psql** can access postgres via haproxy on default port 5432:

Use **make ips** to find out the ip of the proxy.

```bash
$ psql -U user -d postgres -h 172.20.0.4 -p 5432 -c "SELECT * from users;"
```

```bash
$ docker exec -it client1 psql -h proxy -p 5432 -U user -d postgres -W -c "SELECT * from USERS;"
```


For continuous testing: 

```bash
docker exec -it client1 bash -c 'while true; do psql -h proxy -p 5432 -U postgres -d testdb -W -c "SELECT 1;" ; sleep 1; done'
```

Remove packet loss: 

```bash
./scripts/remove-loss.sh <container_name> (e.g., db-primary)
```

## Troubleshooting


### Verify db-primary is healthy

```bash
docker exec db-primary pg_isready -U postgres -p 5433.
```

### Test DNS: 

```bash
docker exec db-secondary ping db-primary.
```

### Ensure no local Postgres conflicts

Stop local Postgres (sudo systemctl stop postgresql) 
or
check ports (netstat -tuln | grep '5433\|5434').


### POSTGRES_PASSWORD not set warning

Ensure POSTGRES_PASSWORD=password is set in docker-compose.yml and Makefile.
Check environment: echo $POSTGRES_PASSWORD.

Run make cleanup and make all to reset.


### HAProxy issues (e.g., "could not resolve address")

Check docker logs proxy for errors.
Verify configs/haproxy.cfg has correct ports (5433 for db-primary, 5434 for db-secondary).
Inspect config: docker exec proxy cat /usr/local/etc/haproxy/haproxy.cfg.
Test DNS: docker exec proxy ping db-secondary.
Restart proxy: docker-compose restart proxy.


### tc errors (e.g., "RTNETLINK: Operation not permitted")

Ensure NET_ADMIN capability in docker-compose.yml.
Verify target IP: docker inspect proxy | grep IPAddress.
Check network interface: docker exec <container_name> ip link (should show eth0).
Debug tc: docker exec <container_name> tc -s qdisc show dev eth0.


### Log management

- View logs: **make logs** or docker logs <container_name>.
- Clean logs: **make clean-logs** to truncate container logs.


### Replication status

Check replication slot: 

```bash
docker exec db-primary psql -U postgres -p 5433 -c "SELECT * FROM pg_replication_slots;" 
```
(look for active=true, non-null restart_lsn).

Check replication status: 

```bash
docker exec db-primary psql -U postgres -p 5433 -c "SELECT * FROM pg_stat_replication;".
```

Monitor with docker logs <service> or tc -s qdisc show dev eth0 inside containers.

## Notes

- Packet loss is simulated unidirectionally using tc with HTB qdisc and filters.
- The custom postgres-iproute2:16 image includes iproute2 for tc commands.
- Replication is basic streaming replication; proxy balances connections (primary active, secondary backup).
- Postgres instances use port 5433 internally
- Postgres instances 
  - use port 5433 (db-primary)
  - use port 5434 (db-secondary) 
  - avoids conflicts with local Postgres (default 5432).
- For real-world use, enhance with tools like repmgr or pgpool.


## Links 
 - https://severalnines.com/resources/whitepapers/postgresql-load-balancing-haproxy/
 - https://medium.com/@eremeykin/how-to-setup-single-primary-postgresql-replication-with-docker-compose-98c48f233bbf
