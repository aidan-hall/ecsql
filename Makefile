CC	= gcc
SRC	:= $(wildcard src/*.c)
SRC	+= $(wildcard deps/*/*.c)

CFLAGS	+= -std=gnu2x

LDLIBS = -lgccjit

OBJ := $(SRC:.c=.o)


ecsql: $(OBJ)
	$(CC) -o $@ $(OBJ) $(LDLIBS)
clean:
	$(RM) -v $(OBJ) ecsql
