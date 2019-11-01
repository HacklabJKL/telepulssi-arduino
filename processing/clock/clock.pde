/**
 * Telepulssi template for Processing.
 */

import java.util.*;
import java.net.*;
import java.io.*;

import java.text.*;

Telepulssi telepulssi;
final static DateFormat timeFmt = new SimpleDateFormat("HHmmss");
final static DateFormat dayFmt = new SimpleDateFormat("E d.M.y");
PImage logo;
PImage degree;
String tempString = "N/A";
boolean noTemp = true;
Timer timer;

public void settings() {
  // Telepulssi screen resolution is 40x7
  size(40, 7);
}

void setup() {  
  // First set up your stuff.
  noStroke();
   TimerTask repeatedTask = new TimerTask() {
        public void run() {
            fetchTemperaturePage();
        }
    };
    timer = new Timer("Timer");
     
    long delay  = 5000L;
    long period = 600000L;
    timer.scheduleAtFixedRate(repeatedTask, delay, period);
  PFont font = loadFont("Ubuntu-Medium-10.vlw");
  textFont(font);
  logo = loadImage("logo.png");
  degree = loadImage("degree.png");

  // If you supply serial port from command-line, use that. Emulate otherwise.
  String port = args == null ? null : args[0];
  telepulssi = new Telepulssi(this, port == null, port); // Preview only

  // Hide the original window
  surface.setVisible(false);
}

void draw() {
  // Clear screen
  background(0);
  fill(255);

  // Angle function which pauses for a moment at zero. Used for pausing to clock position.
  final float phaseShift = PI/2;
  final float speed = 0.0001;
  final int pause = 40;
  float angle = 2*PI*pow(sin((speed*millis()) % (PI/2)), pause) + phaseShift;
  float round = (speed * millis() / (PI/2) % 2) < 1 ? 1 : -1;

  float y = min(-0.5*(sin(-angle)+1)*(logo.height/2-height) * round, 10);
  float x = -0.5*(cos(angle)+1)*(logo.width/2-width);

  // Rotate the whole thing
  translate(x,y);
  
  // Draw clock in some coordinates in the logo
  pushMatrix();
  translate(15, 0);
  drawText();
  popMatrix();

  scale(0.5);
  drawLogo();

  // Finally update the screen and preview.
  telepulssi.update();
}

void drawLogo() {
  image(logo, 0, 0);
}


void drawTemperature() {
  text(tempString, 0, 7);
  image(degree,26,-1);
  text("C",31,7);
}

void drawText() {
  if((millis()/4000)%4==0 && !noTemp){
    drawTemperature();
  }else{
    drawClock();
  }
}

void drawClock() {
  long ts = System.currentTimeMillis();
  String now = format(ts);
  String next = format(ts+1000);
  float phase = (float)(ts % 1000) / 1000;

  // Draw actual digits
  drawDigit(now, next, phase, 0, 0);
  drawDigit(now, next, phase, 1, 6);
  drawDigit(now, next, phase, 2, 14);
  drawDigit(now, next, phase, 3, 20);
  drawDigit(now, next, phase, 4, 28);
  drawDigit(now, next, phase, 5, 34);

  // Blinking digits
  if (ts % 1000 < 500) {
    text(':', 11, 6);
    text(':', 25, 6);
  }

  // Draw nice gradient to rolling numbers
  fill(0);
  rect(0, 7.5, 40, 8);
  rect(0, -8.5, 40, 8);
  
  // Write weekday
  fill(255);
  text(dayFmt.format(new Date(ts)), -13, -3);
}

String format(long ts) {
  String s = timeFmt.format(new Date(ts));
  if ("133700".equals(s)) return "ELITE!";
  if ("140000".equals(s)) return "KAHVIA";
  return s;
}

void drawDigit(String a, String b, float phase, int i, float pos) {
  float textPhase;
  if (a.charAt(i) == b.charAt(i)) {
    // Position static
    textPhase = 0;
  } else {
    // Use textPhase which stops for a moment
    textPhase = phase < 0.5 ? 0 : (phase-0.5)*2;
  }

  pushMatrix();
  translate(pos, -textPhase*8);
  text(a.charAt(i), 0, 7);
  text(b.charAt(i), 0, 15);
  popMatrix();
}

void fetchTemperaturePage(){
  String content = null;
  URLConnection connection = null;
  try {
    connection =  new URL("http://weather.jyu.fi").openConnection();
    Scanner scanner = new Scanner(connection.getInputStream());
    scanner.useDelimiter("\\Z");
    content = scanner.next();
    scanner.close();
  }catch ( Exception ex ) {
    // noconnection
    noTemp = true;
  }
  try{
    int indexOfTemp = content.indexOf("font-size:20px; strong")+25;
    tempString =content.substring(indexOfTemp,indexOfTemp+5);
    noTemp = false;
    //System.out.println(tempString);
  }catch(Exception ex){
    noTemp = true;
  }
}
