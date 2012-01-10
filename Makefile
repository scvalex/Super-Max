all: build

.PHONY: run build clean

run: build
	@echo
	@echo Running Super Max
	build/super-max

build: build/CMakeCache.txt
	make -C build

build/CMakeCache.txt:
	mkdir -p build
	cd build && cmake ..

clean:
	rm -rf build
