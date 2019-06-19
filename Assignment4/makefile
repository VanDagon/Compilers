.SUFFIXES:
CC     = gcc
LEX    = flex
YACC   = bison

CFLAGS = -Wall -DLEXDEBUG -g -pedantic -std=c99
YFLAGS = -d -v
LFLAGS = -t

YFILES = minako-syntax.y
LFILES = minako-lexic.l
CFILES =

SOURCE = $(YFILES) $(LFILES) $(CFILES)
TARGET = $(YFILES:%.y=%.tab.o) $(LFILES:%.l=%.o) $(CFILES:%.c=%.o)

# Compiling
%.tab.c %.tab.h: %.y
	$(YACC) $(YFLAGS) $<
%.c: %.l
	$(LEX) $(LFLAGS) $< > $@
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Targets
all: minako

minako: $(TARGET)
	$(CC) $(CFLAGS) $^ -o $@

run: minako
	./minako demorgan.c1

clean:
	$(RM) $(RMFILES) minako core *.o *.tab.* *.output
