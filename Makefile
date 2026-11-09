.PHONY: all clean test library-on-demand

ifeq ($(DEBUG),1)
BUILD_TYPE := Debug
BUILD_DIR := upstream/_build-debug
STACK_TEST := stack --work-dir .stack-work-debug test --extra-lib-dirs=$(BUILD_DIR)/library
else
BUILD_TYPE := Release
BUILD_DIR := upstream/_build
STACK_TEST := stack test
endif

LIBRARY_OPTIONS := \
  -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
  -DTRACY_ONLY_LOCALHOST=ON \
  -DTRACY_ENABLE=ON \
  -DTRACY_MANUAL_LIFETIME=ON \
  -DTRACY_NO_CRASH_HANDLER=ON

test: $(BUILD_DIR)/library/libTracyClient.a $(BUILD_DIR)/capture/tracy-capture
	killall tracy-capture || true
	$(BUILD_DIR)/capture/tracy-capture -f -o output.tracy &
	$(STACK_TEST)
	sleep 1 && echo Done

all: $(BUILD_DIR)/library/libTracyClient.a $(BUILD_DIR)/capture/tracy-capture $(BUILD_DIR)/profiler/tracy-profiler

clean:
	rm -rf upstream/_build upstream/_build-debug

upstream/tracy/.git:
	git clone --recursive --branch v0.15.0 https://github.com/wolfpld/tracy upstream/tracy

$(BUILD_DIR)/library/Makefile: | upstream/tracy/.git
	cmake -B $(BUILD_DIR)/library -S upstream/tracy $(LIBRARY_OPTIONS)

$(BUILD_DIR)/library/libTracyClient.a: $(BUILD_DIR)/library/Makefile
	cmake --build $(BUILD_DIR)/library --config $(BUILD_TYPE) --parallel

library-on-demand: $(BUILD_DIR)/library-on-demand/libTracyClient.a

$(BUILD_DIR)/library-on-demand/Makefile: | upstream/tracy/.git
	cmake -B $(BUILD_DIR)/library-on-demand -S upstream/tracy $(LIBRARY_OPTIONS) -DTRACY_ON_DEMAND=ON

$(BUILD_DIR)/library-on-demand/libTracyClient.a: $(BUILD_DIR)/library-on-demand/Makefile
	cmake --build $(BUILD_DIR)/library-on-demand --config $(BUILD_TYPE) --parallel

$(BUILD_DIR)/capture/Makefile: | upstream/tracy/.git
	cmake -B $(BUILD_DIR)/capture -S upstream/tracy/capture -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)

$(BUILD_DIR)/capture/tracy-capture: $(BUILD_DIR)/capture/Makefile
	cmake --build $(BUILD_DIR)/capture --config $(BUILD_TYPE) --parallel

$(BUILD_DIR)/profiler/Makefile: | upstream/tracy/.git
	cmake -B $(BUILD_DIR)/profiler -S upstream/tracy/profiler -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)

$(BUILD_DIR)/profiler/tracy-profiler: $(BUILD_DIR)/profiler/Makefile
	cmake --build $(BUILD_DIR)/profiler --config $(BUILD_TYPE) --parallel
