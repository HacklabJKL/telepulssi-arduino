# Telepulssi Oy TP-KN10

Display with 8 LED segment displays of size 5x7. Reverse engineered by kahvikello & Zouppen.

Slave board pinout:

Pin | Telepulssi header            | Pro Micro pin
--- | ---------------------------- | --------------
 1  | 5V                           | Vcc
 2  | 5V                           | Vcc
 3  | GND                          | Gnd
 4  | GND                          | Gnd
 5  | IC 22 OUTPUT ENABLE          | 2
 6  | IC 21 OUTPUT ENABLE          | A3
 7  | IC 20 OUTPUT ENABLE          | 3
 8  | IC 19 OUTPUT ENABLE          | A2
 9  | IC 18 OUTPUT ENABLE          | 4
10  | IC 17 OUTPUT ENABLE          | A1
11  | IC 16 OUTPUT ENABLE          | 5
12  | IC 15 OUTPUT ENABLE          | A0
13  | ROW LATCH (IC 14)            | 6
14  | COLUMN LATCH (IC 15-22)      | 8
15  | SHIFT CLOCK (IC 14-22)       | 15 (SCK)
16  | SERIAL DATA INPUT (IC 14-22) | 16 (MOSI)

Max clock frequency 24 MHz
