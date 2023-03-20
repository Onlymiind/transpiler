.RECIPEPREFIX = >

clean:
> rm -r build

config_debug:
> mkdir -p build && cd build && cmake -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

config_release:
> mkdir -p build && cd build && cmake -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..

transpiler:
> $(MAKE) -C build transpiler

transpiler_debug: config_debug
> $(MAKE) -C build transpiler

transpiler_release: config_release
> $(MAKE) -C build transpiler

all:
>$(MAKE) -C build all

debug: config_debug
> $(MAKE) -C build all

release: config_release
> $(MAKE) -C build all

tests: config_debug
> $(MAKE) -C build tests

