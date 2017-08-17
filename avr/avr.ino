/* -*- mode: c++; c-file-style: "linux" -*-
  Telepulssi TP-KN10 slave LED screen driver
*/

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
volatile uint16_t pwm_lengths[] = { 200, 100, 50 }; // PWM cycle lengths in 0.5µs
uint8_t pwm_i = 0; // Current PWM cycle
volatile uint8_t spi_row_change = false;
volatile uint8_t driving_readiness = 1;

// Hardware configuration
#define PIN_COL_BIT0 A1
#define PIN_COL_BIT1 A2
#define PIN_COL_BIT2 A3
#define PIN_ROW_LATCH 10
#define PIN_COL_LATCH A0

// Serial access
unsigned int byte_i;
uint8_t bit_i;

inline static void try_buf_swap(void);
inline static void frame_store(uint8_t intensity);
inline static void set_pixel(uint8_t *plane);
static void screen_on(void);
inline static void pick_column(void);
static void try_drive_screen(void);

inline static void try_buf_swap(void) {
	if (!may_flip) return;
	uint8_t (*tmp)[56] = buf_front;
	buf_front = buf_back;
	buf_back = tmp;
	Serial.write('S');
	may_flip = 0;
}

void setup() {
	// Enable output pins for LED display
	pinMode(PIN_COL_BIT0, OUTPUT);
	pinMode(PIN_COL_BIT1, OUTPUT);
	pinMode(PIN_COL_BIT2, OUTPUT);
	pinMode(9, OUTPUT); // OC1A main screen turn on
	pinMode(PIN_ROW_LATCH, OUTPUT);
	pinMode(PIN_COL_LATCH, OUTPUT);
	pinMode(MOSI, OUTPUT);
	pinMode(SCK, OUTPUT);

	// Initialize serial via USB
	Serial.begin(19200);

	// Configure SPI as master, clock rate fck/16, enable interrupts	
	SPCR = _BV(SPIE) | _BV(SPE) | _BV(MSTR) | _BV(SPR0);
	
	// Timer magic
	cli();
	TCCR1A = 0;
	TCCR1B = _BV(CS11); // Clock divider 8, 8/16e6 = 500ns tick
	TCNT1 = 0; // Set timer counter initial value (16 bit value)
	TIMSK1 = _BV(OCIE1A); // Go to interrupt vector when timer reaches target
	OCR1A = 1; // Go to the interrupt vector ASAP after startup
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
		if (byte_i<6) {
			// Receive PWM plane durations as 3 big endian 16 bit values.
			uint8_t i = byte_i >> 1;
			pwm_lengths[i] = pwm_lengths[i] << 8 | in;
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
	// Screen is off when we hit here (OC1A has been set LOW)
	try_drive_screen();
}

static void try_drive_screen(void)
{
	// Do not drive until both timer interrupt and SPI interrupt
	// have completed.
	if (++driving_readiness != 2) return;
	driving_readiness = 0;

	// Load new data to LED driver
	digitalWrite(PIN_COL_LATCH, HIGH);
	
	// Are we moving to next segment?
	if (pwm_i == 0) {
		if (col_i == 0) {
			// Switch the row on row driver first and
			// postpone turning on the screen.
			spi_row_change = true;
			SPDR = 1 << row_i;
			pick_column();
		} else {
			// Switch column and turn on the screen.
			pick_column();
			screen_on();
		}
      	} else {
		// Just turn on the screen, going to next PWM plane.
		screen_on();
	}
}

inline static void pick_column(void)
{
	digitalWrite(PIN_COL_BIT2, col_i & 0b100);
	digitalWrite(PIN_COL_BIT1, col_i & 0b010);
	digitalWrite(PIN_COL_BIT0, col_i & 0b001);
}

static void screen_on(void)
{
	// Main screen turn on! (Use Compare Output magic with OC1A)
	TCCR1A = _BV(COM1A1) | _BV(COM1A0); // Set HIGH on match
	TCCR1C = _BV(FOC1A); // Force match (turns the screen on)
	TCCR1A = _BV(COM1A1); // On timer match set turn off the screen.
  
	// Restart timer counter
	TCNT1 = 0;
	
	// Set run length
	OCR1A = pwm_lengths[pwm_i];

	// We have done all time-critical stuff for now
	sei();
	
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
	pwm_i++;
	if (pwm_i >= ARRAY_SIZE(pwm_lengths)) {
		pwm_i = 0;
		col_i++;
		if (col_i >= 8) {
			col_i = 0;
			row_i++;
			if (row_i >= 7) {
				row_i = 0;
				try_buf_swap();
			}
			// We can already pull the row latch down
			digitalWrite(PIN_ROW_LATCH, LOW);
		}
	}

	// Column driver latches on rising edge so we can already pull
	// it down.
	digitalWrite(PIN_COL_LATCH, LOW);
	
	// Send new data via SPI. Completion is monitored in SPI_STC_vect.
	SPDR = buf_front[pwm_i][8*row_i+col_i];
}

// SPI interrupt is only used when changing the row. Writes to LED
// register are guaranteed (I hope) to complete in time.
ISR(SPI_STC_vect)
{
	if (spi_row_change) {
		// Latch the written row info
		digitalWrite(PIN_ROW_LATCH, HIGH);
		spi_row_change = false;
		screen_on();
	} else {
		try_drive_screen();
	}
}
