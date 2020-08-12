decker-engine := ~/.local/bin/decker-engine

directory := $(PWD)
pidfile := decker-engine.pid
lockfile := decker-engine.lock

build:
	stack build

install:
	stack install
	systemctl --user restart decker-engine

open: build
	open -a safari http://localhost:8081/static/index.html
	sleep 1
	stack run -- decker-engine

install-service: build
	mkdir -p /home/henrik/.config/systemd/user
	cp decker-engine.service /home/henrik/.config/systemd/user
	systemctl --user daemon-reload

start: install-service
	systemctl --user start decker-engine

stop: install-service
	systemctl --user stop decker-engine

status: install-service
	systemctl --user status decker-engine

restart: install-service
	systemctl --user restart decker-engine

daemon: build kill
	daemonize -c $(directory) -p $(pidfile) -l $(lockfile) $(decker-engine)

kill:
	if [ -f $(pidfile) ]; then \
		kill `cat $(pidfile)` && rm -f $(lockfile) $(pidfile); \
	fi
