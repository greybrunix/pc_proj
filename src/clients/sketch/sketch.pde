/*
 * Linear Interpolation with Angle Control and Visual Indicator (Using keyPressed()).
 * 
 * Control the angle of the ball with left and right arrow keys,
 * and move it in the direction it's pointing with the up arrow key,
 * with lerping to smooth out the movement.
 */

float x;
float y;
float angle = 0;
float playerAngle;
float playerX;
float playerY;
float speed = 30;
float easing = 0.015;
float easingAngle = 0.2;

void setup() {
  size(1920, 1080);
  noStroke();
  playerX = x = width / 4;
  playerY = y = height / 4;
  playerAngle = angle;
}

void draw() {
  background(51);

  // Calculate velocity components based on the angle
  float vx = cos(angle) * speed;
  float vy = sin(angle) * speed;

  // Update position using lerping for smooth movement
  x = lerp(x,playerX, easing);
  y = lerp(y,playerY, easing);

  // Draw visual indicator for the angle
  float lineLength = 10; // Length of the visual indicator line
  float lineEndX = x + cos(angle) * lineLength;
  float lineEndY = y + sin(angle) * lineLength;
  stroke(255);
  line(x, y, lineEndX, lineEndY);

  // Draw the ball
  fill(255);
  ellipse(x, y, 15, 15);

  //Draw the sun
  ellipse(width/2,height/2,300,300);

}

void keyPressed() {
  // Control the angle with left and right arrow keys
  if (keyCode == LEFT) {
    playerAngle -= 0.2;
    angle = lerp(angle, playerAngle, easingAngle);
  }
  if (keyCode == RIGHT) {
    playerAngle += 0.2;
    angle = lerp(angle, playerAngle, easingAngle);
  }

  // Update target position based on the angle when the up arrow key is pressed
  if (keyCode == UP) {
    playerX += cos(angle) * speed;
    playerY += sin(angle) * speed;
  }

}

