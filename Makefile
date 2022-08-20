.PHONY: build
build:
	$(MAKE) build-static || exit
	$(MAKE) build-app || exit

.PHONY: build-app
build-app:
	stack build
	stack exec webgl-fundamentals-fp clean
	stack exec webgl-fundamentals-fp build

.PHONY: build-static
build-static:
	yarn --cwd ./static build

.PHONY: start
start:
	$(MAKE) build-static
	stack build
	stack exec webgl-fundamentals-fp-exe

.PHONY: watch
watch:
	find ./src ./static/src -type f -name '*.hs' -o -name "*.purs" -o -name "*.css" | entr -r $(MAKE) start

