# Telepulssi TP-KN10 driver for Arduino

Local radio amateur club OH6AD has "bonked" some 80s display modules
built by Telepulssi Oy. [Zouppen](https://twitter.com/zouppen) and
kahvikello accepted the challenge of reverse engineering them to
display animations via USB and developed this driver.

See [Youtube video](https://www.youtube.com/watch?v=fZEshGM8jz8) of
the screen in action.

## Building

Open Arduino IDE and build [avr.ino](avr/avr.ino). Supported
microcontrollers include all ATMega328 and ATMega32u4 models. Can
easily ported to other MCUs. Change pin definitions in the source if
your hardware is different from ours.

To see how to wire TP-KN10 to Arduino, see [pinout](pinout.md).

## Processing

[Processing](https://processing.org/) is the most fun way to control
Telepulssi. Processing version 3.3 or newer is recommended.

Open
[telepulssi_processing.pde](telepulssi_processing/telepulssi_processing.pde)
to Processing, change serial port name, hack and run! :-)

## UDP interface

When running UDP interface, first set serial port settings (replace
serial port to match yours):

```sh
stty -F /dev/ttyACM0 raw 19200
```

### Start UDP server

```sh
cd tools
runghc UdpServer.hs >/dev/ttyACM0
```

TODO: Make Cabal package for UdpServer to make it run more easily.

### Send PNG images

Send any image over UDP port to this screen by running:

```sh
nc -uw0 SERVER_IP 1337 <your_image.png
```

### Naïve clock

```sh
./effects/clock /dev/ttyUSB0
```

## Missing features

1. Cabal package for UDP server
1. Schematics
