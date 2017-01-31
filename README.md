## Examples

First, set serial port speed:

```sh
stty -F /dev/ttyUSB0 raw 9600
```

Naïve clock:

```sh
{
        echo -en '\xff'
        while true; do
                sleep 1&
                convert -background white -gravity center +antialias +dither \
                        -font Ubuntu -pointsize 6 "pango:`date +%H:%M:%S`" \
                        -gravity north -crop '40x7+1+1!' -threshold 50% \
                        -flatten -gravity west -splice 3x0+40+0 \
                        -splice 3x0+35+0 -splice 3x0+30+0 -splice 3x0+25+0 \
                        -splice 3x0+20+0 -splice 3x0+15+0 -splice 3x0+10+0 \
                        -splice 3x0+5+0 -flatten mono:-
                wait
        done
} >/dev/ttyUSB0
```
