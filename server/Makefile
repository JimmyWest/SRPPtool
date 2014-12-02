ERLC=/usr/bin/erlc
ERLCFLAGS=-o
SRCDIR=./src
INCFLAG=-I
INCDIR=./include
LOGDIR=./build/log
BEAMDIR=./build/ebin

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) -v -Werror $(ERLCFLAGS) $(BEAMDIR) -I $(INCDIR) $(SRCDIR)/*.erl ;
	@ mkdir -p $(LOGDIR) ;

kind:
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) -v $(ERLCFLAGS) $(BEAMDIR) -I $(INCDIR) $(SRCDIR)/*.erl ;
	@ mkdir -p $(LOGDIR) ;

clean: 
	@ rm -rf ./build
