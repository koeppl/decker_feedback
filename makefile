decker-engine := ~/.local/bin/decker-engine

directory := $(PWD)
pidfile := decker-engine.pid
lockfile := decker-engine.lock

build-all: build
	decker html

build:
	stack build

runerver: build
	run -- decker-engine

run: build
	stack run -- decker-engine

install:
	stack install

open: build
	(sleep 2; open -a firefox http://localhost:8081/api-test.html)&
	stack run -- decker-engine

install-service: install
	sudo systemctl stop decker-engine
	sudo cp decker-engine.service /etc/systemd/system
	sudo cp ~/.local/bin/decker-engine /usr/local/bin
	sudo cp ~/.local/bin/decker-daemon /usr/local/bin
	sudo cp -r static/* /var/local/decker/static
	sudo chown -R decker:decker /var/local/decker
	sudo systemctl daemon-reload
	sudo systemctl enable decker-engine
	sudo systemctl start decker-engine


daemon: build kill
	daemonize -c $(directory) -p $(pidfile) -l $(lockfile) $(decker-engine)

kill:
	if [ -f $(pidfile) ]; then \
		kill `cat $(pidfile)` && rm -f $(lockfile) $(pidfile); \
	fi
