EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:atreus64
LIBS:testi-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L HE10-16 P1
U 1 1 5990061F
P 7250 3100
F 0 "P1" H 7250 3550 50  0000 C CNN
F 1 "HE10-16" H 7250 2650 50  0000 C CNN
F 2 "Connectors:IDC_Header_Straight_16pins" H 7250 3100 50  0001 C CNN
F 3 "" H 7250 3100 50  0000 C CNN
	1    7250 3100
	1    0    0    -1  
$EndComp
$Comp
L ARDUINO_PRO_MICRO U1
U 1 1 59900E4F
P 3150 3450
F 0 "U1" V 3250 3400 60  0000 C CNN
F 1 "ARDUINO_PRO_MICRO" V 3150 3400 60  0000 C CNN
F 2 "Atreus62:ARDUINO_PRO_MICRO" H 3000 3950 60  0001 C CNN
F 3 "" H 3000 3950 60  0000 C CNN
	1    3150 3450
	1    0    0    -1  
$EndComp
NoConn ~ 2550 3550
NoConn ~ 2550 3650
NoConn ~ 3750 2650
NoConn ~ 3750 2750
NoConn ~ 2550 2650
NoConn ~ 2550 2750
$Comp
L GND #PWR01
U 1 1 59902176
P 2550 4150
F 0 "#PWR01" H 2550 3900 50  0001 C CNN
F 1 "GND" H 2550 4000 50  0000 C CNN
F 2 "" H 2550 4150 50  0000 C CNN
F 3 "" H 2550 4150 50  0000 C CNN
	1    2550 4150
	1    0    0    -1  
$EndComp
$Comp
L 74LS138 U2
U 1 1 5990037E
P 5100 3200
F 0 "U2" H 5200 3700 50  0000 C CNN
F 1 "74LS138" H 5250 2651 50  0000 L CNN
F 2 "Housings_DIP:DIP-16_W7.62mm" H 5100 3200 50  0001 C CNN
F 3 "" H 5100 3200 50  0000 C CNN
	1    5100 3200
	1    0    0    -1  
$EndComp
NoConn ~ 3750 3050
$Comp
L +5V #PWR02
U 1 1 5990B6EF
P 5100 2750
F 0 "#PWR02" H 5100 2600 50  0001 C CNN
F 1 "+5V" H 5100 2890 50  0000 C CNN
F 2 "" H 5100 2750 50  0001 C CNN
F 3 "" H 5100 2750 50  0001 C CNN
	1    5100 2750
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR03
U 1 1 5990B70F
P 2550 3250
F 0 "#PWR03" H 2550 3100 50  0001 C CNN
F 1 "+5V" H 2550 3390 50  0000 C CNN
F 2 "" H 2550 3250 50  0001 C CNN
F 3 "" H 2550 3250 50  0001 C CNN
	1    2550 3250
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR04
U 1 1 5990B7F6
P 6650 2750
F 0 "#PWR04" H 6650 2600 50  0001 C CNN
F 1 "+5V" H 6650 2890 50  0000 C CNN
F 2 "" H 6650 2750 50  0001 C CNN
F 3 "" H 6650 2750 50  0001 C CNN
	1    6650 2750
	1    0    0    -1  
$EndComp
$Comp
L +5V #PWR05
U 1 1 5990B813
P 7850 2750
F 0 "#PWR05" H 7850 2600 50  0001 C CNN
F 1 "+5V" H 7850 2890 50  0000 C CNN
F 2 "" H 7850 2750 50  0001 C CNN
F 3 "" H 7850 2750 50  0001 C CNN
	1    7850 2750
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR06
U 1 1 5990B9CB
P 4500 3850
F 0 "#PWR06" H 4500 3600 50  0001 C CNN
F 1 "GND" H 4500 3700 50  0000 C CNN
F 2 "" H 4500 3850 50  0001 C CNN
F 3 "" H 4500 3850 50  0001 C CNN
	1    4500 3850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR07
U 1 1 5990BAD2
P 6150 2850
F 0 "#PWR07" H 6150 2600 50  0001 C CNN
F 1 "GND" H 6150 2700 50  0000 C CNN
F 2 "" H 6150 2850 50  0001 C CNN
F 3 "" H 6150 2850 50  0001 C CNN
	1    6150 2850
	1    0    0    -1  
$EndComp
$Comp
L GND #PWR08
U 1 1 5990BAEF
P 7850 2850
F 0 "#PWR08" H 7850 2600 50  0001 C CNN
F 1 "GND" H 7850 2700 50  0000 C CNN
F 2 "" H 7850 2850 50  0001 C CNN
F 3 "" H 7850 2850 50  0001 C CNN
	1    7850 2850
	0    -1   -1   0   
$EndComp
NoConn ~ 3750 2950
NoConn ~ 3750 2850
NoConn ~ 3750 3650
Text GLabel 3750 3550 2    55   Output ~ 0
DATA
Text GLabel 7850 3450 2    55   Input ~ 0
DATA
Text GLabel 3750 3750 2    55   Output ~ 0
CLK
Text GLabel 6650 3450 0    55   Input ~ 0
CLK
Text GLabel 3750 3950 2    55   Output ~ 0
COL
Text GLabel 6650 3350 0    55   Input ~ 0
COL
Text GLabel 3750 3450 2    55   Output ~ 0
ROW
Text GLabel 7850 3350 2    55   Input ~ 0
ROW
NoConn ~ 3750 3250
NoConn ~ 3750 3150
Wire Wire Line
	2550 3950 2550 4150
Connection ~ 2550 3950
Wire Wire Line
	7850 2850 7850 2850
Connection ~ 2550 4150
Wire Wire Line
	6650 2850 6150 2850
Wire Wire Line
	4500 3450 4500 3850
Connection ~ 4500 3550
Wire Wire Line
	7850 3250 8500 3250
Wire Wire Line
	5700 3150 6650 3150
Wire Wire Line
	5800 2500 8400 2500
Wire Wire Line
	8400 2500 8400 3150
Wire Wire Line
	8400 3150 7850 3150
Wire Wire Line
	5900 3250 5900 3650
Wire Wire Line
	5900 3650 8200 3650
Wire Wire Line
	8300 3750 8300 2950
Wire Wire Line
	8300 2950 7850 2950
Wire Wire Line
	3750 3350 4500 3350
Connection ~ 4500 3850
Connection ~ 2550 4050
Wire Wire Line
	5700 2850 5700 2400
Wire Wire Line
	5700 2400 8500 2400
Wire Wire Line
	8500 2400 8500 3250
Wire Wire Line
	5700 2950 6000 2950
Wire Wire Line
	6000 2950 6000 3250
Wire Wire Line
	6000 3250 6650 3250
Wire Wire Line
	5800 2500 5800 3050
Wire Wire Line
	5800 3050 5700 3050
Wire Wire Line
	5900 3250 5700 3250
Wire Wire Line
	5700 3450 5800 3450
Wire Wire Line
	5800 3450 5800 3750
Wire Wire Line
	5800 3750 8300 3750
Wire Wire Line
	6350 3550 5700 3550
Wire Wire Line
	6650 3050 6250 3050
Wire Wire Line
	6250 3050 6250 3350
Wire Wire Line
	6250 3350 5700 3350
Wire Wire Line
	6350 3550 6350 2950
Wire Wire Line
	6350 2950 6650 2950
Wire Wire Line
	5100 3650 5100 3850
Wire Wire Line
	5100 3850 4500 3850
Wire Wire Line
	8200 3650 8200 3050
Wire Wire Line
	8200 3050 7850 3050
Wire Wire Line
	4200 2850 4200 4050
Wire Wire Line
	4200 2850 4500 2850
Wire Wire Line
	4300 2950 4300 4150
Wire Wire Line
	4300 2950 4500 2950
Wire Wire Line
	4400 3050 4400 4250
Wire Wire Line
	4400 3050 4500 3050
Wire Wire Line
	4400 4250 3750 4250
Wire Wire Line
	4300 4150 3750 4150
Wire Wire Line
	4200 4050 3750 4050
$EndSCHEMATC
