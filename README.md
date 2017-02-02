# Telepulssi TP-KN10 driver for Arduino

Local radio amateur club OH6AD has "bonked" some 90s display modules
built by Telepulssi Oy. [Zouppen](https://twitter.com/zouppen) and
kahvikello accepted the challenge of reverse engineering them to
display animations via USB and developed this driver.

See [Youtube video](https://www.youtube.com/watch?v=fZEshGM8jz8) of
the screen in action.

## Building

Open Arduino IDE and build
[avr.ino](avr/avr.ino). Supported
microcontrollers include all ATMega328 models. Can easily ported to
other MCUs.

To see how to wire TP-KN10 to Arduino, see [pinout](pinout.md).

TODO better building instructions for Arduino.

## Examples

These examples require `imagemagick` package. It is used for text
rendering and for some binary arithmetic :-D

If running data manually (without the supplied scripts, you need to
set serial port speed:

```sh
stty -F /dev/ttyUSB0 raw 9600
```

Naïve clock:

```sh
./effects/clock /dev/ttyUSB0
```

Show user supplied line on the LED screen:

```sh
{
        echo -en '\xff'
        while read -r line; do
                convert -background white -gravity center +antialias +dither \
                        -font Ubuntu -pointsize 6 -define pango:markup=false \
                        "pango:$line" \
                        -gravity north -crop '40x7+1+1!' -threshold 50% \
                        -flatten -gravity west -splice 3x0+40+0 \
                        -splice 3x0+35+0 -splice 3x0+30+0 -splice 3x0+25+0 \
                        -splice 3x0+20+0 -splice 3x0+15+0 -splice 3x0+10+0 \
                        -splice 3x0+5+0 -flatten mono:-
        done
} >/dev/ttyUSB0
```

Naïve scroller (FIXME: dies at the end of message):

```sh
{
        echo -en '\xff'
        for pos in `seq 1 1000`; do
                sleep 0.1&
                convert -background white -gravity northwest +antialias +dither \
                        -font Ubuntu -pointsize 6 \
                        "pango:Terveisiä Jyväskylän Hacklabilta. Nyt on `date '+%d.%m.%Y %H:%M:%S'`. Moimoi!" \
                        -splice 40x0+0+0 -crop "40x7+$pos+1!" -threshold 50% \
                        -flatten -splice 3x0+40+0 -splice 3x0+35+0 \
                        -splice 3x0+30+0 -splice 3x0+25+0 -splice 3x0+20+0 \
                        -splice 3x0+15+0 -splice 3x0+10+0 -splice 3x0+5+0 \
                        mono:-
                wait
        done
} >/dev/ttyUSB0
```
