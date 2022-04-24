SHELL = /bin/sh

HSFLAGS := -dynamic -O1 -fmax-errors=2
GHC_WARNS := -Wall -Wextra -Wmissing-exported-signatures -Widentities \
             -Wpartial-fields -Wredundant-constraints
GHC_EXTS := -XOverloadedStrings -XLambdaCase -XStrictData \
            -XScopedTypeVariables -XImportQualifiedPost


CACHE := cache
HI_DIR := $(CACHE)/hi_files
OBJ_DIR := $(CACHE)/obj_files


GHC_FLAGS := -hidir $(HI_DIR) -odir $(OBJ_DIR) -isrc/ \
             $(GHC_WARNS) $(GHC_EXTS) $(HSFLAGS)


.PHONY: all
all: bin/mads-server bin/mads-client


bin/mads-client: src/mads-client.hs src/MADS/Client/*
bin/mads-server: src/mads-server.hs src/MADS/Server/*

bin/mads-client bin/mads-server: Settings.hs | bin $(CACHE)
	ghc $(GHC_FLAGS) -o $@ $(patsubst bin/%,src/%.hs,$@)
	@rm -f $(HI_DIR)/Main.hi
	@rm -f $(OBJ_DIR)/Main.o


bin $(CACHE):
	mkdir $@


.PHONY: clean
clean:
	rm -fr bin/ $(CACHE)
