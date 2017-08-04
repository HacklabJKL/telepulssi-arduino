/**
 * Telepulssi template for Processing.
 * Use your mouse pointer to draw pixels on Telepulssi
 */

void setup() {
  // Telepulssi screen is 40x7 but the extra space is used for preview.
  size(730, 140);
  // Initialize real Telepulssi. Comment out if you want to run preview only.
  initTelepulssi("/dev/ttyACM0");
  // Serial traffic distorts the PWM so we don't want to do it too often.
  frameRate(30);
  
  // Do your magic.
  background(0);
}

void draw() {
  // Do your magic.
  stroke(255);
  if (mousePressed == true) {
    line(mouseX, mouseY, pmouseX, pmouseY);
  }
  
  // Finally update the screen and preview.
  updateTelepulssi();
}