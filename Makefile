HOST = $(shell hostname -s)
SRCS = $(wildcard $(HOST).*)
OBJS = $(patsubst $(HOST).%,dot.%,$(SRCS))

all: $(OBJS)

dot.%: $(HOST).%
	ln -s $< $@

install: all
	./install.py
.PHONY: install
