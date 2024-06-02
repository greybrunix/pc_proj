import com.fasterxml.jackson.core.JsonProcessingException;
import processing.core.PApplet;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.Map;

public class Game {

    PApplet parent;

    String data;

    ObjectMapper objectMapper = new ObjectMapper();

    Map<String, Player> players;
    Map<String, Planet> planets;

    Player me = new Player();

    public Game(PApplet p) {
        parent = p;
    }

    public void setup() {
        try {
            waitGame(Interface.username);
        } catch (IOException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    public void drawGameMenu() {
        parent.background(0);
        parent.noStroke();
        parent.fill(255);
        parent.textSize(64);
        parent.textAlign(parent.LEFT, parent.CENTER);

        if (me.waitingGame) {
            parent.text("Waiting for players to begin game...", (float) (parent.width-950) /2,
                    (float) parent.height /2 );
        } else if (me.inGame) {
            game();
        }
    }

    public void receiveData() throws IOException, InterruptedException {
        new Thread(() -> {
            while (me.inGame || me.waitingGame) {
                try {
                    if (me.waitingGame)
                        data = Interface.receiveData();
                    data = Interface.receiveData();

                    new Thread(() -> {
                        try {
                            GameData gameData = objectMapper.readValue(data, GameData.class);
                            synchronized (this) {
                                planets = gameData.planets;
                                players = gameData.players;
                            }
                            me.inGame = true;
                            me.waitingGame = false;
                        } catch (JsonProcessingException e) {
                            throw new RuntimeException(e);
                        }
                    }).start();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
    }

    public void game() {
        for (Map.Entry<String,Planet> planet : planets.entrySet()) {
            drawBall(planet.getValue().x, planet.getValue().y, planet.getValue().diameter, planet.getValue().r, planet.getValue().g, planet.getValue().b);
        }
        for (Map.Entry<String, Player> player : players.entrySet()) {
            if (player.getValue().waitingGame)
                player.getValue().waitingGame = false;
            if (!player.getValue().hasLost)
                drawPlayer(player.getValue().x, player.getValue().y, player.getValue().diameter, player.getValue().angle, player.getValue().r, player.getValue().b, player.getValue().b,
                        player.getValue().lineEndX, player.getValue().lineEndY);
        }
    }

    public void drawBall(float x, float y, float diameter, float r, float g, float b) {
        parent.fill(r, g, b);
        parent.ellipse(x, y, diameter, diameter);
    }

    public void drawPlayer(float x, float y, float diameter, float angle, float r, float g, float b,
                           float lineEndX, float lineEndY) {
        parent.stroke(255);
        parent.line(x, y, lineEndX, lineEndY);
        drawBall(x, y, diameter, r, g, b);
    }

    public void keyPressed() {
        if (parent.key == parent.LEFT) {
            System.out.println("ESQUERDA");
            Interface.keyPressed(me.username, "LEFT");
        } else if (parent.key == parent.RIGHT) {
            System.out.println("DIREITA");
            Interface.keyPressed(me.username, "RIGHT");
        } else if (parent.key == parent.UP) {
            System.out.println("CIMA");
            Interface.keyPressed(me.username, "UP");
        }
    }

    public void waitGame(String username) throws IOException, InterruptedException {
        Interface.wantPlay(username);
        me.username = username;
        me.waitingGame = true;
        receiveData();
    }
}
