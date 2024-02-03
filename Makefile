BUILDDIR = build

kilo: kilo.c
	mkdir -p $(BUILDDIR) && $(CC) kilo.c -o $(BUILDDIR)/kilo -Wall -Wextra -pedantic -std=c99

clean:
	rm -rf $(BUILDDIR)/* 
