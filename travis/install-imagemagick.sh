#!/bin/sh
VERSION=6.9.3-1
set -ex
wget http://www.imagemagick.org/download/ImageMagick-${VERSION}.tar.gz
tar -xzvf ImageMagick-${VERSION}.tar.gz
cd ImageMagick-${VERSION}  && ./configure --prefix=/usr && make && sudo make install
