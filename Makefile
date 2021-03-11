##
## EPITECH PROJECT, 2019
## FUN_imageCompressor_2019
## File description:
## Makefile
##

RM		=	rm -rf

SRM		=	stack clean --full

BUILD	=	stack build

SCRIPT	=	get_bin.sh

BIN		=	imageCompressor

all:
	$(BUILD)
	./$(SCRIPT)

clean:
	$(SRM)

fclean:	clean
	$(RM) $(BIN)

re: fclean all

.PHONY: all clean fclean re