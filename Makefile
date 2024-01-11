
CC	= clang
SRC	:= $(wildcard src/*.c)
SRC	+= $(wildcard deps/*/*.c)

CFLAGS	+= -std=c2x

OBJ := $(SRC:.c=.o)


ecsql: $(OBJ)
	$(CC) -o $@ $(OBJ)
clean:
	$(RM) -v $(OBJ)
