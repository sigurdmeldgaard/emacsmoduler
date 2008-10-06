#!/usr/bin/env bash
emacs -batch -f batch-byte-compile `find . | grep \.el$`
