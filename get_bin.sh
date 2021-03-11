#!/bin/bash

path="$(stack path --local-install-root)"
path+="/bin/icompressor-exe"
cp ${path} ./
mv icompressor-exe imageCompressor