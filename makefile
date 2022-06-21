SHELL = /bin/sh

HSFLAGS := -dynamic -O1 -fmax-errors=2
GHC_WARNS := -Wall -Wextra -Wmissing-exported-signatures -Widentities \
             -Wpartial-fields -Wredundant-constraints \
             -Wno-unused-imports
GHC_EXTS := -XOverloadedStrings -XLambdaCase -XStrictData \
            -XScopedTypeVariables -XImportQualifiedPost


CACHE := .cache

GHC_FLAGS := -outputdir $(CACHE) -isrc/ \
             $(GHC_WARNS) $(GHC_EXTS) $(HSFLAGS)


.PHONY: all
all: bin/madm-server bin/madm-client


bin/madm-client: src/madm-client.hs src/MADM/Client/*
bin/madm-server: src/madm-server.hs src/MADM/Server/*

bin/madm-client bin/madm-server: Settings.hs | bin $(CACHE)
	ghc $(GHC_FLAGS) -o $@ $(patsubst bin/%,src/%.hs,$@)
	@rm -f $(CACHE)/Main.o $(CACHE)/Main.hi


bin $(CACHE):
	mkdir $@


.PHONY: clean
clean:
	rm -fr bin/ $(CACHE)
