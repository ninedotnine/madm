SHELL = /bin/sh

HSFLAGS := -Wall -Wextra -dynamic -O1 -fmax-errors=2

GHC_EXTS := -XOverloadedStrings -XLambdaCase -XStrictData -XScopedTypeVariables -XImportQualifiedPost

FILES := $(addprefix src/, $(shell ls -1 src/ | grep -v 'mads-'))

HI_DIR := cache/hi_files
OBJ_DIR := cache/obj_files
GHC_FLAGS := -hidir $(HI_DIR) -odir $(OBJ_DIR) -isrc/

.PHONY: all
all: bin/mads-server bin/mads-client

bin/mads-client: src/mads-client.hs $(FILES) | bin cache
	ghc $(HSFLAGS) $(GHC_EXTS) $(GHC_FLAGS) -o $@ $<
	@rm -f cache/$(HI_DIR)/Main.hi
	@rm -f cache/$(OBJ_DIR)/Main.o

bin/mads-server: src/mads-server.hs $(FILES) | bin cache
	ghc $(HSFLAGS) $(GHC_EXTS) $(GHC_FLAGS) -o $@ $<
	@rm -f cache/$(HI_DIR)/Main.hi
	@rm -f cache/$(OBJ_DIR)/Main.o


bin cache:
	mkdir $@

.PHONY: clean
clean:
	rm -fr bin/ cache/
