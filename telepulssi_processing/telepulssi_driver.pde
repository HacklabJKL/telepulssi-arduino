import processing.serial.*;
import java.nio.ByteBuffer;

public class Telepulssi extends PApplet {
  private PApplet source;
  Serial myPort;
  ByteBuffer out = ByteBuffer.allocateDirect(2+2*70);
  boolean serial_ready = false;
  
  Telepulssi(PApplet source, boolean preview, String port) {
    super();
    this.source = source;
    if (preview) {
      // We want to preview
      PApplet.runSketch(new String[] {this.getClass().getSimpleName()}, this);
    }
    
    if (port != null) {
      // We have hardware
      myPort = new Serial(this, port, 19200);
      myPort.write("~P");
      out.put(byte(126));
      out.put(byte(70));
    }
    
    // Serial traffic distorts the PWM so we don't want to do it too often.
    source.frameRate(30);
    frameRate(30);
  }

  void settings() {
    size(800, 140);
  }

  void setup() {
    noStroke();
    println(source.frameRate);
    frameRate(source.frameRate);
    source.loadPixels();
  }

  void draw() {
    // Do not load pixel array, done in updateTelepulssi()
    background(0);
    
    // Calculate proper preview size
    int scaling = min(width/source.width, height/source.height);
    
    // Draw round preview pixels
    for (int y=0; y<source.height; y++) {
      for (int x=0; x<source.width; x++) {
        fill(source.pixels[source.width*y+x] & 0xc0, 0, 0, 255);
        ellipse(scaling*(0.5+x), scaling*(0.5+y), 0.65*scaling, 0.65*scaling);
      }
    }
  }
  
  void serialEvent(Serial myPort) {
    char inByte = myPort.readChar();
    if (inByte!='P' && inByte != 'S') return; // Wait only SYNC events
    serial_ready = true;
  }

  // Update explicitly
  public void update() {
    // Update pixel buffer and preview
    source.loadPixels();
  
    // Continue only if Telepulssi is ready
    if (!serial_ready) return;
    
    // Prepare data
    out.position(2);
    for (int y=0; y<7; y++) {
      for (int x=0; x<40; x+=4) {
        int i = source.width*y+x;
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
  
  private int pixel_2bit(int pixel) {
    return (byte)((source.pixels[pixel] >> 6) & 0x3);
  }
}