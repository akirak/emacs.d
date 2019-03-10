#!/bin/sh
docker run --rm -w /root/.emacs.d -v $PWD:/root/.emacs.d akirak/nixmacs-bootstrap \
       sh -c 'make init && make test'
