CC	= gcc
SRCDIR = src
SRC	:= $(wildcard $(SRCDIR)/*.c)
SRC	+= $(wildcard deps/*/*.c)

CPPFLAGS := -std=gnu2x $(CVERSION)
CFLAGS	+= -g $(CPPFLAGS) -Wall

LDLIBS =

OBJ := $(SRC:.c=.o)
DFILES := $(SRC:.c=.d)

%.d: %.c
	$(CC) -MM $(CPPFLAGS) $< | sed -E 's,(.*)\.o[ :]*,$(SRCDIR)/\1.o $(SRCDIR)/\1.d : ,g' > $@

ecsql: $(OBJ) $(DFILES)
	$(CC) -o $@ $(OBJ) $(LDLIBS)

clean:
	$(RM) -v $(OBJ) $(DFILES) $(wildcard src/*.d.*) $(wildcard src/*.d) $(wildcard a-*.d) ecsql

include $(DFILES)
