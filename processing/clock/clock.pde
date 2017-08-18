/**
 * Telepulssi template for Processing.
 */

import java.util.*;
import java.text.*;

Telepulssi telepulssi;
final static DateFormat fmt = new SimpleDateFormat("HHmmss");
PFont font;

public void settings() {
  // Telepulssi screen resolution is 40x7
  size(40, 7);
}

void setup() {  
  // First set up your stuff.
  background(0);
  font = loadFont("Ubuntu-10.vlw");
  textFont(font);
    
  // Initialize real Telepulssi, emulated one, or both. Pick the on you like to use
  telepulssi = new Telepulssi(this, true, "/dev/ttyACM0"); // Preview and real hardware
  //telepulssi = new Telepulssi(this, true, null); // Preview only
  //telepulssi = new Telepulssi(this, false, "/dev/ttyACM0"); // Real hardware only
  
  // Hide the original window
  surface.setVisible(false);
}

void draw() {
  long ts = System.currentTimeMillis();
  String now = fmt.format(new Date(ts));
  String next = fmt.format(new Date(ts+1000));
  double phase = (double)(ts % 1000) / 1000;
  
  // Do something fancy like rotating text
  background(0);
  fill(255);
  
  // Draw actual digits
  drawDigit(now, next, phase, 0, 0);
  drawDigit(now, next, phase, 1, 6);
  drawDigit(now, next, phase, 2, 14);
  drawDigit(now, next, phase, 3, 20);
  drawDigit(now, next, phase, 4, 28);
  drawDigit(now, next, phase, 5, 34);
  
  // Blinking digits
  if ((int)(ts/1000) % 2 == 0) {
    text(':', 12, 6);
    text(':', 26, 6);
  }

  //println(phase);
  
  // Finally update the screen and preview.
  telepulssi.update();
}

void drawDigit(String a, String b, double phase, int i, int pos) {
  int ya, yb;
  if (a.charAt(i) == b.charAt(i)) {
    // Position static
    ya = 7;
  } else {
    // Use textPhase which stops for a moment
    double textPhase = phase < 0.5 ? 0 : (phase-0.5)*2;
    ya = (int)(textPhase*8)+7;
  }
  yb = ya-8;
  
  text(a.charAt(i), pos, ya);
  if(yb > 0) {
    // Draw next digit only if visible
    text(b.charAt(i), pos, yb);
  }
}