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

MAKNAM = hello.mak

LIBDRS =

INCDRS =

INCDRS = $(ROOTDIR)/include

LIBFLS =

SRCFLS = ex01.c \
				 $(ROOTDIR)/source/hello.c

OBJFLS = ex01.o \
				 $(ROOTDIR)/source/hello.o

EXE = ex01.exe

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

