# Telepulssi Oy TP-KN10

Display with 8 LED segment displays of size 5x7. Reverse engineered by kahvikello & Zouppen.

Slave board pinout:

Pin | Description | Atmega328 pin | Duemilanove/Uno pin
--- | -------------------------- | ---- | ---
 1  | 5V                         |      | 5V
 2  | 5V                         |      | 5V
 3  | GND                        |      | Gnd
 4  | GND                        |      | Gnd
 5  | IC 22 OUTPUT ENABLE        | PC5  | A5
 6  | IC 21 OUTPUT ENABLE        | PC4  | A4
 7  | IC 20 OUTPUT ENABLE        | PC3  | A3
 8  | IC 19 OUTPUT ENABLE        | PC2  | A2
 9  | IC 18 OUTPUT ENABLE        | PC1  | A1
10  | IC 17 OUTPUT ENABLE        | PC0  | A0
11  | IC 16 OUTPUT ENABLE        | PD7  | 7
12  | IC 15 OUTPUT ENABLE        | PD6  | 6
13  | IC 15-22 LATCH CLOCK       | PD5  | 5
14  | IC 14 LATCH CLOCK          | PD4  | 4
15  | IC 14-22 SHIFT CLOCK       | SCK  | 13
16  | IC 14-22 SERIAL DATA INPUT | MOSI | 11

Max clock frequency 24 MHz
