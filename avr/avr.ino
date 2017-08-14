/* -*- mode: c++; c-file-style: "linux" -*-
  Telepulssi TP-KN10 slave LED screen driver
*/

#include <SPI.h>

// Helper functions. NB! ARRAY_SIZE doesn't check if it's an array.
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

// Serial protocol fundamentals
#define ESCAPE          '~'  // Escape character. Go to command mode
#define LITERAL_ESCAPE  '\0' // Escape followed by this is literal escape.
#define CMD_FRAME       'F'  // Incoming frame
#define CMD_PING        'P'  // Serial port ping
#define CMD_PWM         'W'  // PWM parameter setup

#define MODE_NOOP    0 // No operation, ignore input until escape
#define MODE_ESCAPE  1 // Escape received, wait for a command
#define MODE_FRAME   2 // Receiving frame
#define MODE_PWM     3 // PWM parameter setup

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
int pwm_lengths[] = { 0x30, 0x10, 0x06 }; // PWM cycle lengths
volatile int pwm_i = 0; // Current PWM cycle

// Hardware configuration
#define PIN_COL_BIT0 A2
#define PIN_COL_BIT1 A1
#define PIN_COL_BIT2 A0
#define PIN_ROW_LATCH 7
#define PIN_COL_LATCH 8

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
	// Enable output pins for LED display
	pinMode(PIN_COL_BIT0, OUTPUT);
	pinMode(PIN_COL_BIT1, OUTPUT);
	pinMode(PIN_COL_BIT2, OUTPUT);
	pinMode(9, OUTPUT); // OC1A main screen turn on
	pinMode(PIN_ROW_LATCH, OUTPUT);
	pinMode(PIN_COL_LATCH, OUTPUT);

	// Initialize serial and SPI
	Serial.begin(19200);
	SPI.begin();

	// Timer magic
	cli();
	TCCR1A = 0;
	TCCR1B = _BV(CS11) | _BV(CS10); // Clock divider 64, 64/16e6 = 40µs cycle
	TCNT1 = 0; // set timer counter initial value (16 bit value)
	TIMSK1 = _BV(OCIE1A); // enable timer compare match 1A interrupt
	OCR1A = 1; // Go to the interrupt vector ASAP
	sei();

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
		case CMD_PWM:
			mode = MODE_PWM;
			byte_i = 0;
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
	case MODE_PWM:
		if (byte_i<3) {
			pwm_lengths[byte_i] = in;
		} else {
			TCCR1B = in;
			mode = MODE_NOOP;
		}
		byte_i++;
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

ISR(TIMER1_COMPA_vect)
{
	// Force screen off if not yet happened (force OC1A on)
	TCCR1A = (1 << COM1A1);
	TCCR1C = (1 << FOC1A);
  
	// Load new data to LED driver
	digitalWrite(PIN_COL_LATCH, HIGH);
	
	// Are we moving to next segment?
	if (pwm_i == 0) {
		// Switch the row on row driver if needed
		if (col_i == 0) {
			SPI.transfer(1 << row_i);

			// Latch it
			digitalWrite(PIN_ROW_LATCH, LOW);
			digitalWrite(PIN_ROW_LATCH, HIGH);
		}
		// Pick correct column
		digitalWrite(PIN_COL_BIT2, col_i & 0b100);
		digitalWrite(PIN_COL_BIT1, col_i & 0b010);
		digitalWrite(PIN_COL_BIT0, col_i & 0b001);
      	}

	// Main screen turn on! (force OC1A on)
	TCCR1A = (1 << COM1A1) | (1 << COM1A0);
	TCCR1C = (1 << FOC1A);
  
	// Say we want to turn off when we hit the target even if interrupt doesn't run
	TCCR1A = (1 << COM1A1);

	// Restart timer counter
	TCNT1 = 0;
	
	// Set run length
	OCR1A = pwm_lengths[pwm_i];
	
	/* After turning on the screen, we have "plenty" of CPU cycles
	 * to spend. Preparing a new segment to display.
	 *
	 * Iterating order:
	 *
	 * 1. PWM planes of a single LED segment row
	 * 2. Segments rows from left to right
	 * 3. Rows
	 * 4. Flip frame, if available, otherwise; repeat.
	 */
	sei();
	pwm_i++;
	if (pwm_i >= ARRAY_SIZE(pwm_lengths)) {
		pwm_i = 0;
		col_i++;
		if (col_i > 7) {
			col_i = 0;
			row_i++;
			if (row_i > 6) {
				row_i = 0;
				if (may_flip) {
					buf_swap();
				}
			}
		}
	}

	// Column driver latches on rising edge so we can already pull
	// it down.
	digitalWrite(PIN_COL_LATCH, LOW);
	
	// Send new data via SPI. Don't need to wait it to complete
	// because it completes before the next timer interrupt.
	SPDR = buf_front[pwm_i][8*row_i+col_i];
}
