/**
 * Linear Interpolation with Angle Control and Visual Indicator (Using keyPressed()).
 * 
 * Control the angle of the ball with left and right arrow keys,
 * and move it in the direction it's pointing with the up arrow key,
 * with lerping to smooth out the movement.
 */

float x;
float y;
float angle = 0;
float targetAngle;
float targetX;
float targetY;
float speed = 30;
float easing = 0.015;
float easingAngle = 0.2;

void setup() {
  size(1920, 1080);
  noStroke();
  x = width / 2;
  y = height / 2;
  targetX = x;
  targetY = y;
  targetAngle = angle;
}

void draw() {
  background(51);

  // Calculate velocity components based on the angle
  float vx = cos(angle) * speed;
  float vy = sin(angle) * speed;

  // Update position using lerping for smooth movement
  x = lerp(x, targetX, easing);
  y = lerp(y, targetY, easing);

  // Draw visual indicator for the angle
  float lineLength = 50; // Length of the visual indicator line
  float lineEndX = x + cos(angle) * lineLength;
  float lineEndY = y + sin(angle) * lineLength;
  stroke(255);
  line(x, y, lineEndX, lineEndY);

  // Draw the ball
  fill(255);
  ellipse(x, y, 66, 66);
}

void keyPressed() {
  // Control the angle with left and right arrow keys
  if (keyCode == LEFT) {
    targetAngle -= 0.2;
    angle = lerp(angle, targetAngle, easingAngle);
  } else if (keyCode == RIGHT) {
    targetAngle += 0.2;
    angle = lerp(angle, targetAngle, easingAngle);
  }

  // Update target position based on the angle when the up arrow key is pressed
  if (keyCode == UP) {
    targetX += cos(angle) * speed;
    targetY += sin(angle) * speed;
  }
}

