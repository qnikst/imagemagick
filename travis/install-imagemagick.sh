#!/bin/sh
set -ex
wget http://www.imagemagick.org/download/ImageMagick-6.9.1-6.tar.gz
tar -xzvf ImageMagick-6.9.1-6.tar.gz
cd ImageMagick-6.9.1-6  && ./configure --prefix=/usr && make && sudo make install