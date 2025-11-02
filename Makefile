.PHONY: all clean test

test: upstream/_build/capture/tracy-capture
	killall tracy-capture || true
	upstream/_build/capture/tracy-capture -f -o output.tracy &
	stack test
	sleep 1 && echo Done

all: upstream/_build/library/libTracyClient.a upstream/_build/capture/tracy-capture upstream/_build/profiler/tracy-profiler

clean:
	rm -rf upstream/_build

upstream/tracy/.git:
	git clone --recursive https://github.com/wolfpld/tracy

upstream/_build/library/Makefile: upstream/tracy/.git
	cmake -B upstream/_build/library -S tracy \
	  -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
	  -DTRACY_ONLY_LOCALHOST=ON \
	  -DTRACY_ENABLE=ON \
	  -DTRACY_MANUAL_LIFETIME=ON \
	  -DTRACY_DELAYED_INIT=ON

upstream/_build/library/libTracyClient.a: upstream/_build/library/Makefile
	cmake --build upstream/_build/library --config Release --parallel

upstream/_build/capture/Makefile: upstream/tracy/.git
	cmake -B upstream/_build/capture -S tracy/capture

upstream/_build/capture/tracy-capture: upstream/_build/capture/Makefile
	cmake --build upstream/_build/capture --config Release --parallel

upstream/_build/profiler/Makefile: upstream/tracy/.git
	cmake -B upstream/_build/profiler -S tracy/profiler

upstream/_build/profiler/tracy-profiler: upstream/_build/profiler/Makefile
	cmake --build upstream/_build/profiler --config Release --parallel
