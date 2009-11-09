TARGET		:=	osxSetup
SRC		:=	Distribution

HASKELLS	:=	$(shell find $(SRC) -name "*.hs" -or -name "*.lhs")

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

build:			dist/build

lint:
			hlint $(HASKELLS)
