import processing.core.PApplet;

import java.util.ArrayList;

public class Game extends PApplet {

    ArrayList<Player> players = new ArrayList<Player>();
    ArrayList<Planet> planets = new ArrayList<Planet>();

    Player me = new Player();

    public void settings() {
        size(1920, 1080);
        noStroke();
    }

    public void draw() {
        background(0);
        fill(255);
        textSize(64);
        textAlign(LEFT, CENTER);

        receiveData();

        if (me.waitingGame) {
            text("Waiting for players to begin game...", (float) (width-950) /2, (float) height /2 );
        } else if (me.gameStart) {
            gameStart();
        } else if (me.ongoingGame) {
            ongoingGame();
        }
    }

    // TODO Read receive data, block process while data not received
    public void receiveData() {

    }

    public void gameStart() {
        for (Planet planet : planets) {
            drawBall(planet.x, planet.y, planet.diameter, planet.r, planet.g, planet.b);
        }
        for (Player player : players) {
            player.gameStart = false;
            drawPlayer(player.x, player.y, player.diameter, player.angle, player.r, player.g, player.b,
                    player.lineEndX, player.lineEndY);
        }
    }

    public void ongoingGame() {
        for (Planet planet : planets) {
            drawBall(planet.x, planet.y, planet.diameter, planet.r, planet.g, planet.b);
        }
        for (Player player : players) {
            drawPlayer(player.x, player.y, player.diameter, player.angle, player.r, player.g, player.b,
                    player.lineEndX, player.lineEndY);
        }
    }

    public void drawBall(float x, float y, float diameter, int r, int g, int b) {
        fill(r, g, b);
        ellipse(x, y, diameter, diameter);
    }

    public void drawPlayer(float x, float y, float diameter, float angle, int r, int g, int b,
                           float lineEndX, float lineEndY) {
        stroke(255);
        line(x, y, lineEndX, lineEndY);
        drawBall(x, y, diameter, r, g, b);
    }

    public void keyPressed() {
        if (key == LEFT) {
            Interface.keyPressed(me.username, "LEFT");
        } else if (key == RIGHT) {
            Interface.keyPressed(me.username, "RIGHT");
        } else if (key == UP) {
            Interface.keyPressed(me.username, "UP");
        }
    }

    public void waitGame(String username) {
        Interface.wantPlay(username);
        me.username = username;
        me.waitingGame = true;
        run();
    }

    public void run() {
        String[] processingargs = {"Game"};
        PApplet.runSketch(processingargs, Interface.game);
    }
}
