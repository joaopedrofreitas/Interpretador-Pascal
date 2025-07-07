# Compilador Pascal
FPC = fpc

# Diretórios
SRC_DIR = src
LEX_DIR = $(SRC_DIR)/lexical
UTIL_DIR = $(SRC_DIR)/util
SYNTATIC_DIR = $(SRC_DIR)/syntatic
INTERPRETER_DIR = $(SRC_DIR)/interpreter

# Arquivo principal e executável
MAIN = $(SRC_DIR)/main.pas
OUTPUT = pascal-compiler

# Flags de compilação
FLAGS = -Mobjfpc -Fu$(LEX_DIR) -Fu$(UTIL_DIR) -Fu$(SYNTATIC_DIR) -Fu$(INTERPRETER_DIR) -S2 -g -O-

all: compile

compile:
	$(FPC) $(FLAGS) $(MAIN) -o$(OUTPUT)

clean:
	rm -f $(OUTPUT)
	find $(SRC_DIR) -name '*.ppu' -delete
	find $(SRC_DIR) -name '*.o' -delete
	find $(SRC_DIR) -name '*.rsj' -delete

.PHONY: all compile clean
