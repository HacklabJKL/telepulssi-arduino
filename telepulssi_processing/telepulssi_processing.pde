/**
 * Telepulssi template for Processing.
 * Use your mouse pointer to draw pixels on Telepulssi
 */

import processing.serial.*;
import java.nio.ByteBuffer;

Serial myPort;
ByteBuffer out = ByteBuffer.allocateDirect(2+2*70);

void initTelepulssi(String port) {
  myPort = new Serial(this, port, 19200);
  myPort.write("~P");
  out.put(byte(126));
  out.put(byte(70));
}

void serialEvent(Serial myPort) {
  char inByte = myPort.readChar();
  if (inByte!='P' && inByte != 'S') return; // Wait only SYNC events
  // Update pixel buffer
  loadPixels();
  
  // Prepare data
  out.position(2);
  for (int i=0; i<40*7; i+=4) {
    byte b = (byte)((pixel_2bit(i) << 6) | (pixel_2bit(i+1) << 4) | (pixel_2bit(i+2) << 2) | (pixel_2bit(i+3)));
    out.put(b);
    if (b == 126) out.put((byte)0); // Literal escape
  }

  // Extract array and write to serial port.
  byte buf[] = new byte[out.position()];
  out.rewind();
  out.get(buf);
  myPort.write(buf);
}

int pixel_2bit(int pixel) {
  return (byte)((pixels[pixel] >> 6) & 0x3);
}

void setup() {
  // Telepulssi screen must be using of 40x7
  size(40, 7);
  background(0);

  // Initialize Telepulssi
  initTelepulssi("/dev/ttyUSB0");
}

void draw() {
  stroke(255);
  if (mousePressed == true) {
    line(mouseX, mouseY, pmouseX, pmouseY);
  }
}