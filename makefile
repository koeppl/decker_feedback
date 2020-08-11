build:
	stack build

fill:
	curl -X POST -d "First comment!" -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/comments/cgg-1/intro/henrik

list:
	curl -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/comments

