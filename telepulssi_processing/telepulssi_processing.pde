/**
 * Telepulssi template for Processing.
 */

Telepulssi telepulssi;

public void settings() {
  // Telepulssi screen resolution is 40x7
  size(40, 7);
}

void setup() {  
  // First set up your stuff.
  background(0);
    
  // Initialize real Telepulssi, emulated one, or both. Pick the on you like to use
  //telepulssi = new Telepulssi(this, true, "/dev/ttyACM0"); // Preview and real hardware
  telepulssi = new Telepulssi(this, true, null); // Preview only
  //telepulssi = new Telepulssi(this, false, "/dev/ttyACM0"); // Real hardware only
  
  // Hide the original window
  surface.setVisible(false);
}

void draw() {
  // Do something fancy like rotating text
  background(0);
  pushMatrix();
  translate(width*0.5, height*0.5);
  rotate(frameCount / 20.0);
  textSize(7);
  text("Hello", 0, 0);
  popMatrix();
  
  // Finally update the screen and preview.
  telepulssi.update();
}