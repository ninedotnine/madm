SHELL = /bin/sh

HSFLAGS := -Wall -Wextra -dynamic -O1

GHC_EXTS := -XOverloadedStrings -XLambdaCase -XStrictData -XScopedTypeVariables -XImportQualifiedPost -XTupleSections

SRC_DIR := src/
CACHE_DIR := cache
HI_DIR := $(CACHE_DIR)/hi_files
OBJ_DIR := $(CACHE_DIR)/obj_files
GHC_FLAGS := -hidir $(HI_DIR) -odir $(OBJ_DIR) -i$(SRC_DIR)

.PHONY: all
all: bin/mads-server bin/mads-client

bin: 
	mkdir -p bin

bin/mads-client: src/mads-client.hs | bin
	ghc $(HSFLAGS) $(INCLUDES) $(GHC_EXTS) $(GHC_FLAGS) -o $@ $<

bin/mads-server: src/mads-server.hs | bin
	ghc $(HSFLAGS) $(INCLUDES) $(GHC_EXTS) $(GHC_FLAGS) -o $@ $<


.PHONY: clean
clean:
	rm -fr bin/ $(CACHE_DIR)

.PHONY: deps
deps: | $(CACHE_DIR)
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/server-deps src/server.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/expr-deps src/Main_Expr.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/parser-deps src/Main_Parser.hs
	ghc -M -dep-suffix '' $(FLAGS) -dep-makefile $(CACHE_DIR)/typechecker-deps src/Main_TypeChecker.hs
