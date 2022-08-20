.PHONY: build
build:
	yarn
	yarn build

.PHONY: build-scripts
build-scripts:
	rm -rf ./output
	rm -rf ./public/scripts
	spago bundle-app --main Pages.Home.Main --to ./public/scripts/pages/home/index.js
	spago bundle-app --main Pages.Shaders.Animation.Main --to ./public/scripts/pages/shaders/animation/index.js
	spago bundle-app --main Pages.Shaders.Camera.Main --to ./public/scripts/pages/shaders/camera/index.js
	spago bundle-app --main Pages.Shaders.CameraLookAt.Main --to ./public/scripts/pages/shaders/camera-look-at/index.js
	spago bundle-app --main Pages.Shaders.Fudge.Main --to ./public/scripts/pages/shaders/fudge/index.js
	spago bundle-app --main Pages.Shaders.LightingDirectional.Main --to ./public/scripts/pages/shaders/lighting-directional/index.js
	spago bundle-app --main Pages.Shaders.Rectangle.Main --to ./public/scripts/pages/shaders/rectangle/index.js
	spago bundle-app --main Pages.Shaders.Perspective.Main --to ./public/scripts/pages/shaders/perspective/index.js
	spago bundle-app --main Pages.Shaders.Shape3d.Main --to ./public/scripts/pages/shaders/shape3d/index.js
	spago bundle-app --main Pages.Shaders.Texture2d.Main --to ./public/scripts/pages/shaders/texture2d/index.js

.PHONY: build-styles
build-styles:
	yarn postcss ./src/styles/index.css -d ./public/styles

.PHONY: watch
watch:
	find ./src -type f -name '*.purs' -o -name "*.css" | entr -r $(MAKE) build

