SRC = logo-source.png

all: logo.ico logo.png icons

# icotool is part of Debian package icooutils
logo.ico: $(SRC)
	for i in 16 32 48; do \
	  convert -background none -resample $${i}x$${i} $^ logo$${i}.png; \
	done
	icotool -c logo16.png logo32.png logo48.png -o $@
	rm logo16.png logo32.png logo48.png

logo.png: $(SRC)
	convert -background none -resample 64x64 $(SRC) $@

ICONS_SRC = $(wildcard icons/*.svg)
ICONS = $(ICONS_SRC:.svg=.png)

icons/%.png: icons/%.svg
	convert -background none -resample 22x22 $^ $@

clean:
	rm -f $(ICONS) logo.ico logo.png

.PHONY: icons
icons: $(ICONS)
