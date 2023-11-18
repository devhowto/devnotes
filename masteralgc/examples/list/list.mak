CC = gcc
LL = gcc
CFLAGS = -std=c99 \
				 -Og \
				 -g3 \
				 -Wall \
				 -Wextra \
				 -Wpedantic
				 # -fsanitize=address,undefined

LFLAGS =

MAKNAM = list.mak

LIBDRS =

INCDRS =

INCDRS = $(ROOTDIR)/include

LIBFLS =

SRCFLS = list_run.c \
				 $(ROOTDIR)/source/list.c

OBJFLS = list_run.o \
				 $(ROOTDIR)/source/list.o

EXE = list.run

$(EXE): $(OBJFLS)
	$(LL) $(LFLAGS) -o $@ $(OBJFLS) $(LIBDRS) $(LIBFLS)

.c.o:
	$(CC) $(CFLAGS) -o $@ -c -I$(INCDRS) $<

all:
	make -f $(MAKNAM) clean
	make -f $(MAKNAM) depend

depend:
	makedepend -I$(INCDRS) -f $(MAKNAM) $(SRCFLS)
	make -f $(MAKNAM) $(EXE)

clean:
	-rm $(EXE)
	-rm $(OBJFLS)

