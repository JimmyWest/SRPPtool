ERLC=/usr/bin/erlc
ERLCFLAGS=-o
SRCDIR=./src
INCFLAG=-I
INCDIR=./include
LOGDIR=./build/log
BEAMDIR=./build/bin

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) -v $(ERLCFLAGS) $(BEAMDIR) -I $(INCDIR) $(SRCDIR)/*.erl ;
	@ mkdir -p $(LOGDIR) ;
clean: 
	@ rm -rf $(BEAMDIR)
