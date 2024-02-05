CC	= gcc
SRCDIR	= src
SRCS	:= $(wildcard $(SRCDIR)/**/*.c)

TARGET = ecsql

CPPFLAGS := -std=gnu2x $(CVERSION)
CFLAGS	+= $(CPPFLAGS) -Wall

LDLIBS =

OBJDIR := obj
OBJS = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRCS))
DFILES = $(OBJS:.o=.d)

$(OBJDIR)/%.o: $(SRCDIR)/%.c | $(OBJDIR)/%.d
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(OBJDIR)/%.d: $(SRCDIR)/%.c
	mkdir -p $(@D)
	$(CC) -MM $(CPPFLAGS) $< | sed -E 's,(.*)\.o[ :]*,$(@D)/\1.o $(@D)/\1.d : ,g' > $@

debug: CFLAGS += -g
debug: $(TARGET)

release: CFLAGS += -O2 -Wall -Wextra -Werror
release: $(TARGET)

$(TARGET): $(OBJS) $(DFILES)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(OBJS) $(LDLIBS)

clean:
	$(RM) -rv $(OBJDIR)/* ecsql

include $(DFILES)
