/**
 * Linear Interpolation with Key Control (Using keyPressed() and keyReleased()). 
 * 
 * Move the ball across the screen using arrow keys with lerping.  
 */

float x;
float y;
float targetX;
float targetY;
float easing = 0.015; // Adjust the easing value for smoother movement
float speed = 40; // Adjust the speed of movement

void setup() {
  size(1920, 1080); 
  noStroke();  
  x = width / 2; // Start at the center of the screen
  y = height / 2;
  targetX = x;
  targetY = y;
}

void draw() { 
  background(51);
  
  // Use lerping to smoothly move towards the target position
  x = lerp(x, targetX, easing);
  y = lerp(y, targetY, easing);
  
  fill(255);
  ellipse(x, y, 66, 66);
}

void keyPressed() {
  // Update the target position based on key presses
  if (keyCode == UP) {
    targetY = max(0, targetY - speed);
  } else if (keyCode == DOWN) {
    targetY = min(height, targetY + speed);
  } else if (keyCode == LEFT) {
    targetX = max(0, targetX - speed);
  } else if (keyCode == RIGHT) {
    targetX = min(width, targetX + speed);
  }
}

