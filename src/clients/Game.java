import processing.core.PApplet;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

public class Game extends PApplet {

    String data;

    ObjectMapper objectMapper = new ObjectMapper();

    Map<String, Player> players;
    Map<String, Planet> planets;


    Player me = new Player();

    public void settings() {
        size(1920, 1080);
        //noStroke();
    }

    public void draw() {
        background(0);
        fill(255);
        textSize(64);
        textAlign(LEFT, CENTER);

        if (me.waitingGame) {
            text("Waiting for players to begin game...", (float) (width-950) /2, (float) height /2 );
        } else if (me.game) {
            game();
        }
    }

    // TODO Read receive data, block process while data not received
    public void receiveData() throws IOException, InterruptedException {
        //data = Interface.receiveData();
        data = "{"
                + "\"planets\": {"
                + "    \"1\": {"
                + "        \"distance\": 0,"
                + "        \"x\": 1080,"
                + "        \"y\": 720,"
                + "        \"diameter\": 200,"
                + "        \"r\": 255,"
                + "        \"g\": 255,"
                + "        \"b\": 0"
                + "    }"
                + "},"
                + "\"players\": {"
                + "    \"username\": {"
                + "        \"level\": 0,"
                + "        \"victories_in_row\": 0,"
                + "        \"loses_in_row\": 0,"
                + "        \"x\": 1000,"
                + "        \"y\": 500,"
                + "        \"diameter\": 100,"
                + "        \"targetX\": 0,"
                + "        \"targetY\": 0,"
                + "        \"angle\": 0,"
                + "        \"lineLenght\": 10,"
                + "        \"lineEndX\": 0,"
                + "        \"lineEndY\": 0,"
                + "        \"targetAngle\": 0,"
                + "        \"easingAngle\": 0,"
                + "        \"r\": 100,"
                + "        \"g\": 110,"
                + "        \"b\": 110,"
                + "        \"fuel\": 0,"
                + "        \"waitingGame\": false,"
                + "        \"game\": false,"
                + "        \"gameOver\": false,"
                + "        \"gameWon\": false"
                + "    }"
                + "}"
                + "}";
        try {
            GameData gameData = objectMapper.readValue(data, GameData.class);
            planets = gameData.planets;
            players = gameData.players;
        } finally {
          //  while () {
          //      wait();
          //  }
        }
    }

    public void game() {
        for (Map.Entry<String,Planet> planet : planets.entrySet()) {
            drawBall(planet.getValue().x, planet.getValue().y, planet.getValue().diameter, planet.getValue().r, planet.getValue().g, planet.getValue().b);
        }
        for (Map.Entry<String, Player> player : players.entrySet()) {
            if (player.getValue().waitingGame)
                player.getValue().waitingGame = false;
            if (!player.getValue().gameOver)
                drawPlayer(player.getValue().x, player.getValue().y, player.getValue().diameter, player.getValue().angle, player.getValue().r, player.getValue().b, player.getValue().b,
                        player.getValue().lineEndX, player.getValue().lineEndY);
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

    public void waitGame(String username) throws IOException, InterruptedException {
        Interface.wantPlay(username);
        me.username = username;
        //me.waitingGame = true;
        me.waitingGame = false;
        me.game = true;
        receiveData();
        // TODO Create thread that is always reading the values sent from the server and updating the arraylists
        run();
    }

    public void run() {
        String[] processingargs = {"Game"};
        PApplet.runSketch(processingargs, Interface.game);
    }
}