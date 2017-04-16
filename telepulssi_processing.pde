/**
 * Telepulssi template for Processing.
 * Use your mouse pointer to draw pixels on Telepulssi
 */

import processing.serial.*;

Serial myPort;
int pwmPass=0;

void initTelepulssi(String port) {
  myPort = new Serial(this, port, 115200);
  myPort.write('S');
}

void serialEvent(Serial myPort) {
  final int pwmTable[] = {0, 1, 2, 4}; // Hard coded PWM duty cycle length table
  char inByte = myPort.readChar();
  if (inByte!='R' && inByte != 'S') return; // Wait only SYNC events

  // Load new pixels only on start of the PWM cycle
  if (pwmPass == 0) {
    loadPixels();
  }
  
  byte[] out = new byte[7*8];
  for (int i=0; i<7*8; i++) {
    int pos = i*5;
    byte b = 0;
    for (int j=0; j<5; j++) {
      int brightness = (pixels[pos+j] >> 6) & 0x3;
      if (pwmPass < pwmTable[brightness]) b |= 1 << j;
    }
    out[i]=b;
  }
  myPort.write(out);
  pwmPass = (pwmPass+1) & 0x3;
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