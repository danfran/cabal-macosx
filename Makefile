TARGET		:=	cabal-macosx
SRC		:=	Distribution

HASKELLS	:=	$(shell find $(SRC) -name "*.hs" -or -name "*.lhs")
HADDOCKS	:=	dist/doc/html/$(TARGET)

clean:
			@find . -name '*~' -exec rm -vf {} ';' && \
			  find . -name '*.hi' -exec rm -vf {} ';' && \
			  find . -name '*.o' -exec rm -vf {} ';' && \
			  runghc Setup clean

nuke:			clean

dist/setup-config:	$(TARGET).cabal
			runghc Setup configure

dist/build:		$(HASKELLS) dist/setup-config
			runghc Setup build


configure:		$(HASKELLS) dist/setup-config

$(HADDOCKS)/index.html:	configure
			runghc Setup haddock --hyperlink-source

haddock:		$(HADDOCKS)/index.html

haddockall:		configure
			runghc Setup haddock --hyperlink-source --internal

build:			dist/build


sdist:			dist/build
			runghc Setup sdist

view:			haddock
			open $(HADDOCKS)/index.html

lint:
			hlint $(HASKELLS)

.PHONY: clean configure build haddock sdist view lint nuke
