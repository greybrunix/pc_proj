import processing.core.PApplet;

import java.io.IOException;

public class GameOrLeaderboard extends PApplet {

    String username;

    int gameButtonWidth = 300;
    int gameButtonHeight = 200;
    int leaderboardButtonWidth = 300;
    int leaderboardButtonHeight = 200;

    float gameButtonX;
    float gameButtonY;
    float leaderboardButtonX;
    float leaderboardButtonY;

    public void settings() {
        size(1920, 1080);

        gameButtonX = (float) (width - gameButtonWidth - 500) / 2;
        gameButtonY = (float) (height - gameButtonHeight) / 2;
        leaderboardButtonX = (float) (width - leaderboardButtonWidth + 500) / 2;
        leaderboardButtonY = (float) (height - leaderboardButtonHeight) / 2;
    }

    public void draw() {
        background(184);
        fill(0);
        textSize(32);
        textAlign(LEFT, CENTER);

        // Draw game button
        fill(255);
        rect(gameButtonX, gameButtonY, gameButtonWidth, gameButtonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Play Game", gameButtonX + (float) gameButtonWidth /2, gameButtonY + (float) gameButtonHeight /2);

        // Draw leaderboard button
        fill(255);
        rect(leaderboardButtonX, leaderboardButtonY, leaderboardButtonWidth, leaderboardButtonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Leaderboard", leaderboardButtonX + (float) leaderboardButtonWidth / 2, leaderboardButtonY + (float) leaderboardButtonHeight / 2);
    }

    public void mousePressed() {
        if (mouseX >= gameButtonX && mouseX <= gameButtonX + gameButtonWidth &&
                mouseY >= gameButtonY && mouseY <= gameButtonY + gameButtonHeight) {
            try {
                Interface.game.waitGame(username);
            } catch (IOException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        } else if (mouseX >= leaderboardButtonX && mouseX <= leaderboardButtonX + leaderboardButtonWidth &&
                    mouseY >= leaderboardButtonY && mouseY <= leaderboardButtonY + leaderboardButtonHeight) {
            try {
                Interface.leaderboard.askReceiveLeaderboard();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public void run(String user) {
        username = user;
        String[] processingargs = {"GameOrLeaderboard"};
        PApplet.runSketch(processingargs, Interface.gameOrLeaderboard);
    }
}
