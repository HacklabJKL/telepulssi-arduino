/* -*- mode: c++; c-file-style: "linux" -*-
  Telepulssi TP-KN10 slave LED screen driver
*/

#include <SPI.h>
#include <TimerOne.h>

static uint8_t buf_a[] = {
	0x01, 0x00, 0x08, 0x04, 0x10, 0x00, 0x0A, 0x04, 0x01, 0x00, 0x08, 0x04,
	0x10, 0x00, 0x08, 0x04, 0x07, 0x07, 0x0B, 0x05, 0x17, 0x03, 0x0A, 0x05,
	0x09, 0x14, 0x18, 0x04, 0x14, 0x04, 0x1A, 0x04, 0x09, 0x17, 0x18, 0x04,
	0x17, 0x04, 0x1A, 0x04, 0x09, 0x07, 0x0B, 0x0D, 0x17, 0x13, 0x0A, 0x1D,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00
};

static uint8_t buf_b[sizeof(buf_a)];

uint8_t *buf_front = buf_a;
uint8_t *buf_back = buf_b;
uint8_t col_i = 0;
uint8_t row_i = 0;
int buf_i = 0;

void buf_swap(void) {
	uint8_t *tmp = buf_front;
	buf_front = buf_back;
	buf_back = tmp;
}

void setup() {
	// Set output pins for LED display
	DDRC = 0xFF;
	DDRD = 0xFF;
	// Initialize serial
	Serial.begin(9600);
	// initialize SPI
	SPI.begin();
	Timer1.initialize(150);
	Timer1.attachInterrupt(driveDisplay);
}

void loop() {
	// Get a byte from serial port if available.
	if (!Serial.available()) return;
	uint8_t data = Serial.read();
	
	// In case of an escape, stop receiving current frame.
	if (data >= 0x20) {
		buf_i = 0;
		Serial.write('R');
		return;
	}

	// Store the data to back buffer
	buf_back[buf_i++] = data;
	
	// If we have reached the end of frame, flip buffers
	if (buf_i == sizeof(buf_a)) {
		buf_i = 0;
		buf_swap();
		Serial.write('K');
	}
}

void driveDisplay() {
	// Main screen turn off.
	PORTC = 0xFF;
	PORTD = 0xFF; // Latch data on pin 5 at the same time

	// Switch the row on row driver if needed
	if (col_i == 0) {
		SPI.transfer(1 << row_i);

		// Latch it
		digitalWrite(4, LOW);
		digitalWrite(4, HIGH);
	}
	
	// Main screen turn on!
	if (col_i < 2) {
		PORTD = ~((1 << 6) << col_i);
	} else {
		PORTC = ~(1 << (col_i-2));
	}
	
	// After turning on the screen, we have "plenty" of CPU cycles
	// to spend. Preparing a new segment to display.
	
	// Go to the next column. If reached the end of line, jump to next.
	col_i++;
	if (col_i > 7) {
		col_i = 0;
		row_i++;
		if (row_i > 6) row_i = 0;
	}

	// Column driver latches on rising edge so we can already pull
	// it down.
	digitalWrite(5, LOW);
	
	// Send new data via SPI. Don't need to wait it to complete
	// because it completes before the next timer interrupt.
	SPDR = buf_front[8*row_i+col_i];
}
