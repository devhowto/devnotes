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

MAKNAM = factor.mak

LIBDRS =

INCDRS =

INCDRS = $(ROOTDIR)/include

LIBFLS = -lm

SRCFLS = factor_run.c \
				 $(ROOTDIR)/source/factor.c

OBJFLS = factor_run.o \
				 $(ROOTDIR)/source/factor.o

EXE = factor.run

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

