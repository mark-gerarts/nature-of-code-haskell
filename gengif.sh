#!/bin/bash

if [ $# -ne 2 ]
  then
    echo "Command called with invalid number of arguments. Usage:"
    echo ""
    echo "  ./gengif.sh sketch.mp4 sketch.gif"
    echo ""
    exit 1
fi

TARGET=$1
OUTPUT=$2

ffmpeg -i $TARGET -filter_complex "[0:v] palettegen" palette.png -y
ffmpeg -i $TARGET -i palette.png -filter_complex "[0:v][1:v] paletteuse" -r 30 $OUTPUT -y
rm palette.png
