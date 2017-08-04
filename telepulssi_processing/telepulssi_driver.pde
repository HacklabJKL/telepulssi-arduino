import processing.serial.*;
import java.nio.ByteBuffer;

Serial myPort;
ByteBuffer out = ByteBuffer.allocateDirect(2+2*70);
boolean serial_ready = false;

void initTelepulssi(String port) {
  myPort = new Serial(this, port, 19200);
  myPort.write("~P");
  out.put(byte(126));
  out.put(byte(70));
}

void serialEvent(Serial myPort) {
  char inByte = myPort.readChar();
  if (inByte!='P' && inByte != 'S') return; // Wait only SYNC events
  serial_ready = true;
}

void updateTelepulssi() {
  // Update pixel buffer and preview
  loadPixels();
  updatePreview();
  
  // Continue only if Telepulssi is ready
  if (!serial_ready) return;
  
  // Prepare data
  out.position(2);
  for (int y=0; y<7; y++) {
    for (int x=0; x<40; x+=4) {
      int i = width*y+x;
      byte b = (byte)((pixel_2bit(i) << 6) | (pixel_2bit(i+1) << 4) | (pixel_2bit(i+2) << 2) | (pixel_2bit(i+3)));
      out.put(b);
      if (b == 126) out.put((byte)0); // Literal escape
    }
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

void updatePreview() {
  // Calculate proper preview size
  int scaling = min(width/40, (height/7)-1);
  noStroke();
  // Clean preview background
  fill(0);
  rect(0, 7, width, height-7);
  rect(40, 0, width-40, 7);
  
  // Draw round preview pixels
  for (int y=0; y<7; y++) {
    for (int x=0; x<40; x++) {
      int i = width*y+x;
      fill(pixels[i] & 0xc0, 0, 0, 255);
      ellipse(scaling*(0.5+x), 7+scaling*(0.5+y), 0.65*scaling, 0.65*scaling);
    }
  }
}