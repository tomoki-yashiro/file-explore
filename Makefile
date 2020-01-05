# -*- mode: makefile-gmake -*-

ELISP	= file-explorer.el
TARGET	= ${ELISP:.el=.elc}

.PHONY: all build clean

all: build

build: ${TARGET}

clean:
	${RM} ${TARGET}

%.elc: %.el
	emacs --batch -l folder-mode/folder-mode.el -f batch-byte-compile $<
