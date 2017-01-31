
/*
  Telepulssi TP-KN10 slave LED screen driver
*/

#include <SPI.h>
#include <TimerOne.h>

static uint8_t koira14[] = {
	0x01, 0x00, 0x08, 0x04, 0x10, 0x00, 0x0A, 0x04, 0x01, 0x00, 0x08, 0x04,
	0x10, 0x00, 0x08, 0x04, 0x07, 0x07, 0x0B, 0x05, 0x17, 0x03, 0x0A, 0x05,
	0x09, 0x14, 0x18, 0x04, 0x14, 0x04, 0x1A, 0x04, 0x09, 0x17, 0x18, 0x04,
	0x17, 0x04, 0x1A, 0x04, 0x09, 0x07, 0x0B, 0x0D, 0x17, 0x13, 0x0A, 0x1D,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00
};

void setup() {
	// Set output pins for LED display
	DDRC = 0xFF;
	DDRD = 0xFF;
	// Initialize serial
	Serial.begin(9600);
	// initialize SPI
	SPI.begin();
	Timer1.initialize(200);
	Timer1.attachInterrupt(driveDisplay);
}

uint8_t col_i = 0;
uint8_t row_i = 0;
int buf_i = 0;

void loop() {
	if (Serial.available() > 0) {
                // read the incoming byte
                uint8_t data = Serial.read();
		if (data > 32) {
			// If sending escape, reset buffer to beginning
			buf_i = 0;
			Serial.write('R');
			return;
		}
		
		koira14[buf_i++] = data;
		if (buf_i == sizeof(koira14)) {
			buf_i = 0;
			Serial.write('K');
		}
        }
}

void driveDisplay() {
	// Main screen turn off
	PORTC = 0xFF;
	PORTD = 0xFF; // Latch data on pin 5 at the same time

	if (col_i == 0) {
		SPI.transfer(1 << row_i);

		// Latch it
		digitalWrite(4, LOW);
		digitalWrite(4, HIGH);
	}
	
	// Main screen turn on.
	if (col_i < 2) {
		PORTD = ~((1 << 6) << col_i);
	} else {
		PORTC = ~(1 << (col_i-2));
	}
	
	// Prepare new data
	col_i++;
	
	// Advance to next row if needed
	if (col_i > 7) {
		col_i = 0;
		row_i++;
		if (row_i > 6) row_i = 0;
	}

	// Pull down latch
	digitalWrite(5, LOW);
	
	// Send new data to SPI. Don't wait it to complete
	SPDR = koira14[8*row_i+col_i];
}
