REBAR = $(shell pwd)/rebar
.PHONY: deps

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

xref: all
	$(REBAR) skip_deps=true xref

