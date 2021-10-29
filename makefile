decker-engine := ~/.local/bin/decker-engine

directory := $(PWD)
pidfile := decker-engine.pid
lockfile := decker-engine.lock

build: 
	stack build

readme:
	pandoc --standalone --self-contained -M css:README.css -M 'title:Decker Engine' README.md -o static/index.html

run-local-cors: build
	DECKER_CORS_ORIGINS="http://127.0.0.1:5500,http://0.0.0.0:8888,http://localhost:8081,http://localhost:8888" stack run -- decker-engine

run-local: build
	stack run -- decker-engine

install:
	stack install

open: build
	(sleep 2; open -a firefox http://localhost:8081/api-test.html)&
	stack run -- decker-engine

install-service: install
	sudo systemctl stop decker-engine
	sudo cp decker-engine.service /etc/systemd/system
	sudo cp ~/.local/bin/decker-engine-exe /usr/local/bin
	sudo cp -r static/* /var/local/decker/static
	sudo cp db/users.yaml /var/local/decker/db
	sudo chown -R decker:decker /var/local/decker
	sudo systemctl daemon-reload
	sudo systemctl enable decker-engine
	sudo systemctl start decker-engine

service-logs:
	journalctl -u decker-engine.service -f

common := --referer http://localhost:8081 --header "Content-Type: application/json" --header "Accept: application/json" 
heise := 

curl:
	curl ${common} -X GET http://localhost:8081/token
	@echo ""
	curl ${common} -X PUT -d '{"deck": "http://heise.de", "slide": "hurgel", "token": "lorgel"}' http://localhost:8081/comments
	@echo ""
	curl ${common} -X POST -d '{"markdown": "*fuck*", "deck": "http://heise.de", "slide": "lorgel"}' http://localhost:8081/comments
	@echo ""
	curl ${common} -X DELETE -d '{"key": 17, "token": "lorgel"}' http://localhost:8081/comments
	@echo ""

.PHONY: curl install-service open install build run-local run-local-cors
