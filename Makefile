build:
	cd APIServer/ && yarn install && yarn run grunt

docker-graphredex: build
	docker build --build-arg GRAPHREDEX_TYPE="graphredex" -t beardhatcode/graphredex -f Dockerfile .

docker-voyager: build
	docker build --build-arg GRAPHREDEX_TYPE="voyager" -t beardhatcode/voyager -f Dockerfile .

docker: docker-graphredex docker-voyager


