CC	= gcc
SRCDIR = src
SRCS	:= $(wildcard $(SRCDIR)/*.c)

CPPFLAGS := -std=gnu2x $(CVERSION)
CFLAGS	+= -g $(CPPFLAGS) -Wall

LDLIBS =

OBJDIR := obj
OBJS = $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRCS))
DFILES = $(OBJS:.o=.d)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

$(OBJDIR)/%.d: $(SRCDIR)/%.c
	$(CC) -MM $(CPPFLAGS) $< | sed -E 's,(.*)\.o[ :]*,$(OBJDIR)/\1.o $(OBJDIR)/\1.d : ,g' > $@

ecsql: $(OBJS) $(DFILES)
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $(OBJS) $(LDLIBS)

clean:
	$(RM) -v $(OBJDIR)/* ecsql

include $(DFILES)
