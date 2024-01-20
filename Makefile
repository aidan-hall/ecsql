CC	= gcc
SRC	:= $(wildcard src/*.c)
SRC	+= $(wildcard deps/*/*.c)

CPPFLAGS := -std=gnu2x $(CVERSION)
CFLAGS	+= -g $(CPPFLAGS) -Wall

LDLIBS = -lgccjit -lreadline

OBJ := $(SRC:.c=.o)
DFILES := $(SRC:.c=.d)

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -MM -MMD $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

ecsql: $(OBJ) $(DFILES)
	$(CC) -o $@ $(OBJ) $(LDLIBS)

clean:
	$(RM) -v $(OBJ) $(DFILES) $(wildcard src/*.d.*) $(wildcard src/*.d) $(wildcard a-*.d) ecsql

include $(DFILES)
