import processing.core.PApplet;

public class Game extends PApplet {

    // TODO Create arraylist to store players and their values
    private String user;

    private boolean waitingGame = false;
    private boolean gameStart = false;

    private float lineLenght;
    private float lineEndX;
    private float lineEndY;

    private float angle = 0;
    private float targetAngle;
    private float easingAngle = 0.2F;

    public void settings() {
        size(1920, 1080);
        noStroke();
        targetAngle = angle;
    }

    public void draw() {
        background(0);
        fill(255);
        textSize(64);
        textAlign(LEFT, CENTER);

        if (waitingGame) {
            text("Waiting for players to begin game...", (float) (width-950) /2, (float) height /2 );
        } else if (gameStart) {
            gameStart();
        }
        // TODO Draw all players and planets with updated values
    }

    public void gameStart() {

        // Draw Sun and Planets
        drawBall((float) width /2, (float) height /2, 100, 255, 255, 0);
        drawBall((float) (width+900) /2, (float) (height-320) /2, 50, 255, 255, 255);
        drawBall((float) (width-500) /2, (float) (height+230) /2, 50, 255, 255, 255);
        drawBall((float) (width+650) /2, (float) (height+120) /2, 50, 255, 255, 255);
        drawBall((float) (width+720) /2, (float) (height-710) /2, 50, 255, 255, 255);

        // Draw Players
        // TODO Add for loop to draw all players
        drawPlayer(500, 500, 10, 0, 173, 30, 209);
    }

    public void drawBall(float x, float y, float diameter, int r, int g, int b) {
        fill(r, g, b);
        ellipse(x, y, diameter, diameter);
    }

    public void drawPlayer(float x, float y, float diameter, float angle, int r, int g, int b) {
        stroke(255);
        line(x, y, lineEndX, lineEndY);
        drawBall(x, y, diameter, r, g, b);
    }

    public void keyPressed() {
        if (key == LEFT) {
            Interface.keyPressed(user, "LEFT");
        } else if (key == RIGHT) {
            Interface.keyPressed(user, "RIGHT");
        } else if (key == UP) {
            Interface.keyPressed(user, "UP");
        }
    }

    public void waitGame(String username) {
        Interface.wantPlay(username);
        user = username;
        // TODO Check if game is received before changing to game
        // waitingGame = true;
        gameStart = true;
        run();
    }

    public void run() {
        String[] processingargs = {"Game"};
        PApplet.runSketch(processingargs, Interface.game);
    }

    // TODO Colisões, Vencedor, Gravidade, Órbita
}
