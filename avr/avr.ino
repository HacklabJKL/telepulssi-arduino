/* -*- mode: c++; c-file-style: "linux" -*-
  Telepulssi TP-KN10 slave LED screen driver
*/

#include <SPI.h>
#include <TimerOne.h>

// Serial protocol fundamentals
#define ESCAPE          '~'  // Escape character. Go to command mode
#define LITERAL_ESCAPE  '\0' // Escape followed by this is literal escape.
#define CMD_FRAME       'F'  // Incoming frame
#define CMD_PING        'P'  // Serial port ping

#define MODE_NOOP    0 // No operation, ignore input until escape
#define MODE_ESCAPE  1 // Escape received, wait for a command
#define MODE_FRAME   2 // Receiving frame

// Hacklab logo at startup
static uint8_t buf_a[3][56] = {
	{ 0x00, 0x00, 0x00, 0x00, 0x1B, 0x1B, 0x1F,
	  0x1E, 0x08, 0x00, 0x00, 0x00, 0x0A, 0x1A,
	  0x10, 0x10, 0x00, 0x00, 0x00, 0x00, 0x02,
	  0x1B, 0x17, 0x12, 0x00, 0x01, 0x00, 0x00,
	  0x03, 0x1B, 0x10, 0x16, 0x00, 0x00, 0x00,
	  0x00, 0x02, 0x1B, 0x16, 0x12, 0x0A, 0x01,
	  0x00, 0x00, 0x0A, 0x12, 0x10, 0x18, 0x00,
	  0x00, 0x00, 0x00, 0x1F, 0x1F, 0x1F, 0x1F, },
	{ 0x00, 0x00, 0x18, 0x1F, 0x1B, 0x1B, 0x1F,
	  0x1E, 0x08, 0x00, 0x00, 0x0C, 0x0A, 0x1A,
	  0x18, 0x18, 0x00, 0x00, 0x10, 0x04, 0x03,
	  0x1B, 0x17, 0x16, 0x00, 0x01, 0x00, 0x14,
	  0x13, 0x1B, 0x11, 0x16, 0x00, 0x00, 0x00,
	  0x05, 0x0B, 0x1B, 0x16, 0x16, 0x0A, 0x01,
	  0x00, 0x04, 0x1A, 0x12, 0x10, 0x18, 0x00,
	  0x00, 0x18, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, },
	{ 0x00, 0x14, 0x1F, 0x1F, 0x1B, 0x1B, 0x1F,
	  0x1E, 0x08, 0x04, 0x06, 0x0E, 0x1A, 0x1B,
	  0x18, 0x18, 0x00, 0x14, 0x1D, 0x15, 0x1B,
	  0x1B, 0x17, 0x16, 0x00, 0x15, 0x0D, 0x14,
	  0x13, 0x1B, 0x11, 0x16, 0x00, 0x14, 0x15,
	  0x15, 0x0B, 0x1B, 0x16, 0x16, 0x0A, 0x15,
	  0x05, 0x0C, 0x1A, 0x17, 0x11, 0x18, 0x00,
	  0x1C, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, 0x1F, },
};

static uint8_t buf_b[3][56];

uint8_t (*buf_front)[56] = buf_a;
uint8_t (*buf_back)[56] = buf_b;
uint8_t col_i = 0;
uint8_t row_i = 0;
volatile uint8_t may_flip = 0;
int pwm_planes[] = {0, 0, 0, 0, 0, 1, 1, 2}; // PWM "plane" running sequence
int pwm_i = 0; // Current PWM cycle
uint8_t *buf_pwm = buf_a[0];

// Serial access
unsigned int byte_i;
uint8_t bit_i;

inline static void frame_store(uint8_t intensity);
inline static void set_pixel(uint8_t *plane);

void buf_swap(void) {
	uint8_t (*tmp)[56] = buf_front;
	buf_front = buf_back;
	buf_back = tmp;
	if (may_flip) {
		Serial.write('S');
		may_flip = 0;
	}
}

void setup() {
	// Set output pins for LED display
	DDRC = 0xFF;
	DDRD = 0xFF;
	// Initialize serial
	Serial.begin(19200);
	// initialize SPI
	SPI.begin();
	Timer1.initialize(50);
	Timer1.attachInterrupt(driveDisplay);
}

void loop() {
	// Get a byte from serial port if available.
	if (!Serial.available()) return;
	uint8_t in = Serial.read();

	static int mode = MODE_NOOP;
	static int old_mode = MODE_NOOP;

	// If escape, store old mode and process no further.
	if (in == ESCAPE) {
		old_mode = mode;
		mode = MODE_ESCAPE;
		return;
	}
	
	// If literal escape, go back to old mode.
	if (mode == MODE_ESCAPE && in == LITERAL_ESCAPE) {
		mode = old_mode;
		in = ESCAPE;
		// Fallthrough after changing mode and input.
	}

	switch (mode) {
	case MODE_ESCAPE:
		switch (in) {
		case CMD_FRAME:
			mode = MODE_FRAME;
			// Zero fill buffer.
			memset(buf_back, 0, sizeof(buf_a));
			byte_i = 0;
			bit_i = 0x01;

			// Do not flip until frame is complete
			may_flip = false;			
			break;
		case CMD_PING:
			mode = MODE_NOOP;
			Serial.write('P');
			break;
		default:
			// Go to no-op mode on invalid char
			mode = MODE_NOOP;
			Serial.write('E');
			break;
		}
		break;
	case MODE_FRAME:
		// Encode PWM from all four 2-bit pixels
		frame_store(in >> 6);
		frame_store((in >> 4) & 0x3);
		frame_store((in >> 2) & 0x3);
		frame_store(in & 0x3);

		// Check if we have reached the end of buffer and ready to flip
		if (byte_i == 56) {
			// Frame ready
			mode = MODE_NOOP;
			may_flip = true;
			Serial.write('K');
		}
		break;
	}
}

inline static void frame_store(uint8_t intensity)
{
	// Fallthrough switch
	switch (intensity) {
	case 3:
		set_pixel(buf_back[0]);
	case 2:
		set_pixel(buf_back[1]);
	case 1:
		set_pixel(buf_back[2]);
	}

	// Advance pointer
	bit_i <<= 1;
	if (bit_i > 0x10) {
		byte_i++;
		bit_i = 0x01;
	}
}

inline static void set_pixel(uint8_t *plane)
{
	plane[byte_i] |= bit_i;
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
		if (row_i > 6) {
			row_i = 0;
			pwm_i++;
			if (pwm_i >= (sizeof(pwm_planes)/sizeof(*pwm_planes))) {
				pwm_i = 0;
				if (may_flip) {
					buf_swap();
				}
			}
			// Switch to next PWM cycle
			buf_pwm = buf_front[pwm_planes[pwm_i]];
		}
	}

	// Column driver latches on rising edge so we can already pull
	// it down.
	digitalWrite(5, LOW);
	
	// Send new data via SPI. Don't need to wait it to complete
	// because it completes before the next timer interrupt.
	SPDR = buf_pwm[8*row_i+col_i];
}
