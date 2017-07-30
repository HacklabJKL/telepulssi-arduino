// -*- mode: c; c-file-style: "linux" -*-

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <strings.h>

// Serial protocol fundamentals
#define ESCAPE	        '~'  // Escape character. Go to command mode
#define LITERAL_ESCAPE  '\0' // Escape followed by this is literal escape.
#define CMD_FRAME       'F'  // Incoming frame

#define MODE_NOOP    0 // No operation, ignore input until escape
#define MODE_ESCAPE  1 // Escape received, wait for a command
#define MODE_FRAME   2 // Receiving frame

// Back buffer where to store PWM passes
uint8_t buf_back[4][8*7];

unsigned int byte_i;
uint8_t bit_i;

void print_plane(uint8_t *plane);
void serial_interrupt(uint8_t in);
inline static void frame_store(uint8_t intensity);
inline static void set_pixel(uint8_t *plane);

int main(int argc, char **argv)
{
	setbuf(stdin, NULL); // Turn off buffering
	
	// Emulate interrupt behaviour
	while (true) {
		int in = getchar();
		if (in == EOF) return 0;
		serial_interrupt(in);
	}
}

void serial_interrupt(uint8_t in)
{
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
			// Zero fill buffer. In production remember to
			// use sizeof(buf_a) because we want array length.
			bzero(buf_back, sizeof(buf_back));
			byte_i = 0;
			bit_i = 0x10;			
			break;
		default:
			// Go to no-op mode on invalid char
			mode = MODE_NOOP;
			putchar('E');
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
			print_plane(buf_back[0]);
			print_plane(buf_back[1]);
			print_plane(buf_back[2]);
			print_plane(buf_back[3]);
		}
		break;
	}
}

inline static void frame_store(uint8_t intensity)
{
	//printf("byte_i %d, bit_i %d\n", byte_i, bit_i);
		
	// Fallthrough switch
	switch (intensity) {
	case 3:
		set_pixel(buf_back[0]);
		set_pixel(buf_back[1]);
	case 2:
		set_pixel(buf_back[2]);
	case 1:
		set_pixel(buf_back[3]);
	}

	// Advance pointer
	bit_i >>= 1;
	if (bit_i == 0x00) {
		byte_i++;
		bit_i = 0x10;
	}
}

inline static void set_pixel(uint8_t *plane)
{
	plane[byte_i] |= bit_i;
}

// Function for validating the results
void print_plane(uint8_t *plane)
{
	for (int y = 0; y < 7; y++) {
	     	for (int x = 0; x < 8; x++) {
			for (uint8_t bit = 0x10; bit > 0; bit >>=1) {
				putchar(plane[8*y+x] & bit ? 'X' : '-');
			}
		}
		putchar('\n');
	}
	putchar('\n');
}
