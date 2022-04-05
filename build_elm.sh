#!/usr/bin/env sh
python tools/csv2elm.py assets/translations -o src-elm/src
cd src-elm && elm make src/Pages/*.elm && mv elm.js ../dist