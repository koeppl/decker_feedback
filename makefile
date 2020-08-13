decker-engine := ~/.local/bin/decker-engine

directory := $(PWD)
pidfile := decker-engine.pid
lockfile := decker-engine.lock

build-all: build
	decker html

build:
	stack build

run: build
	stack run -- decker-engine

install:
	stack install
	systemctl --user restart decker-engine

open: build
	(sleep 2; open -a firefox http://localhost:8081/api-test.html)&
	stack run -- decker-engine

install-service: build
	mkdir -p /home/henrik/.config/systemd/user
	cp decker-engine.service /home/henrik/.config/systemd/user
	systemctl --user stop decker-engine
	systemctl --user daemon-reload
	systemctl --user enable decker-engine
	systemctl --user start decker-engine

start: install-service
	systemctl --user start decker-engine

stop: 
	systemctl --user stop decker-engine

status: 
	systemctl --user status decker-engine

restart: 
	systemctl --user restart decker-engine

daemon: build kill
	daemonize -c $(directory) -p $(pidfile) -l $(lockfile) $(decker-engine)

kill:
	if [ -f $(pidfile) ]; then \
		kill `cat $(pidfile)` && rm -f $(lockfile) $(pidfile); \
	fi
