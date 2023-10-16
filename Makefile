.PHONY: smt-files
smt-files:
	cabal run

.PHONY: verify
verify: smt-files
	z3 SMT/Correctness.smt2
	z3 SMT/ForwardProgress1.smt2
	z3 SMT/ForwardProgress2.smt2

# Docker variables
USER=$(if $(shell id -u),$(shell id -u),9001)
GROUP=$(if $(shell id -g),$(shell id -g),1000)

# Build the docker image
.PHONY: build-docker
build-docker:
	(cd docker; docker build --build-arg UID=$(USER) --build-arg GID=$(GROUP) . --tag five-ubuntu2204)

# Enter the docker image
.PHONY: shell
shell: build-docker
	docker run -it --shm-size 256m --hostname five-ubuntu2204 -u $(USER) -v /home/$(shell whoami)/.ssh:/home/dev-user/.ssh  -v $(shell pwd):/workspace five-ubuntu2204:latest /bin/bash

.PHONY: clean
clean:
	cabal clean
	rm -rf SMT
