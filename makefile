decker-engine := ~/.local/bin/decker-engine

directory := $(PWD)
pidfile := decker-engine.pid
lockfile := decker-engine.lock

build:
	stack build

open: build
	open -a safari http://localhost:8081/static/index.html
	sleep 1
	stack run -- decker-engine

install-service: build
	sudo cp decker-engine.service /etc/systemd/system/
	sudo chmod 644 /etc/systemd/system/decker-engine.service
	sudo systemctl daemon-reload

daemon: build kill
	daemonize -c $(directory) -p $(pidfile) -l $(lockfile) $(decker-engine)

kill:
	if [ -f $(pidfile) ]; then \
		kill `cat $(pidfile)` && rm -f $(lockfile) $(pidfile); \
	fi
