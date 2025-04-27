# Compilador Pascal
FPC = fpc

# Diretórios
SRC_DIR = src
LEX_DIR = $(SRC_DIR)/lexical
UTIL_DIR = $(SRC_DIR)/src/util

# Unidades necessárias
UNITS = \
	$(LEX_DIR)/TokenType.pas \
	$(LEX_DIR)/SymbolUnit.pas \
	$(LEX_DIR)/LexemeUnit.pas \
	$(UTIL_DIR)/MyFile.pas \
	$(LEX_DIR)/lexer.pas

# Arquivo principal e executável
MAIN = $(SRC_DIR)/main.pas
OUTPUT = ../main  # Nome do executável sem aspas

# Flags de compilação
FLAGS = -Mobjfpc -Fu$(LEX_DIR) -Fu$(UTIL_DIR)

all: compile

compile: $(UNITS)
	$(FPC) $(FLAGS) $(MAIN) -o$(OUTPUT)

clean:
	rm -f $(SRC_DIR)/$(OUTPUT)
	find $(SRC_DIR) -name '*.ppu' -delete
	find $(SRC_DIR) -name '*.o' -delete

.PHONY: all compile clean
