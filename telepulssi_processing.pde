/**
 * Telepulssi template for Processing.
 * Use your mouse pointer to draw pixels on Telepulssi
 */

import processing.serial.*;

Serial myPort;

void initTelepulssi(String port) {
  myPort = new Serial(this, port, 115200);
  myPort.write('S');
}

void serialEvent(Serial myPort) {
  char inByte = myPort.readChar();
  if (inByte!='R' && inByte != 'S') return; // Wait only SYNC events
  // Send to Telepulssi
  loadPixels();
  byte[] out = new byte[7*8];
  for (int i=0; i<7*8; i++) {
    int pos = i*5;
    byte b = 0;
    if ((pixels[pos+0] & 0x80) != 0) b |= 1 << 0;
    if ((pixels[pos+1] & 0x80) != 0) b |= 1 << 1;
    if ((pixels[pos+2] & 0x80) != 0) b |= 1 << 2;
    if ((pixels[pos+3] & 0x80) != 0) b |= 1 << 3;
    if ((pixels[pos+4] & 0x80) != 0) b |= 1 << 4;
    out[i]=b;
  }
  myPort.write(out);
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