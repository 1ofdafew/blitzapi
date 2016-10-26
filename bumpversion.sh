#!/bin/sh
#
VERSION=1.0.1

find apps/ -name '*.erl' -exec bumpversion --current-version=$VERSION patch {} --allow-dirty \;
bumpversion --current-version=$VERSION patch rebar.config --allow-dirty 
