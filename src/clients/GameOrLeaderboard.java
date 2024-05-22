import processing.core.PApplet;

import java.io.IOException;

public class GameOrLeaderboard extends PApplet {

    String username;

    int gameButtonWidth = 300;
    int gameButtonHeight = 200;
    int leaderboardButtonWidth = 300;
    int leaderboardButtonHeight = 200;
    int logoutButtonWidth = 200;
    int logoutButtonHeight = 60;
    int deleteAccountButtonWidth = 250;
    int deleteAccountButtonHeight = 60;

    float gameButtonX;
    float gameButtonY;
    float leaderboardButtonX;
    float leaderboardButtonY;
    float logoutButtonX;
    float logoutButtonY;
    float deleteAccountButtonX;
    float deleteAccountButtonY;

    public void settings() {
        size(1920, 1080);

        gameButtonX = (float) (width - gameButtonWidth - 500) / 2;
        gameButtonY = (float) (height - gameButtonHeight) / 2;
        leaderboardButtonX = (float) (width - leaderboardButtonWidth + 500) / 2;
        leaderboardButtonY = (float) (height - leaderboardButtonHeight) / 2;
        logoutButtonX = 20;
        logoutButtonY = 1000;
        deleteAccountButtonX = 1650;
        deleteAccountButtonY = 1000;
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
        text("Play Game", gameButtonX + (float) gameButtonWidth /2,
                gameButtonY + (float) gameButtonHeight /2);

        // Draw leaderboard button
        fill(255);
        rect(leaderboardButtonX, leaderboardButtonY, leaderboardButtonWidth, leaderboardButtonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Leaderboard", leaderboardButtonX + (float) leaderboardButtonWidth / 2,
                leaderboardButtonY + (float) leaderboardButtonHeight / 2);

        // Draw logout button
        fill(255);
        rect(logoutButtonX, logoutButtonY, logoutButtonWidth, logoutButtonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Logout", logoutButtonX + (float) logoutButtonWidth / 2,
                logoutButtonY + (float) logoutButtonHeight / 2);

        // Draw delete account button
        fill(255);
        rect(deleteAccountButtonX, deleteAccountButtonY, deleteAccountButtonWidth, deleteAccountButtonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Delete Account", deleteAccountButtonX + (float) deleteAccountButtonWidth / 2,
                deleteAccountButtonY + (float) deleteAccountButtonHeight / 2);
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
        } else if (mouseX >= logoutButtonX && mouseX <= logoutButtonX + logoutButtonWidth &&
                mouseY >= logoutButtonY && mouseY <= logoutButtonY + logoutButtonHeight) {
            Interface.loginApp.logoutUser(username);
        } else if (mouseX >= deleteAccountButtonX && mouseX <= deleteAccountButtonX + deleteAccountButtonWidth &&
        mouseY >= deleteAccountButtonY && mouseY <= deleteAccountButtonY + deleteAccountButtonWidth) {
            Interface.loginApp.deleteUser(username);
        }
    }

    public void run(String user) {
        username = user;
        String[] processingargs = {"GameOrLeaderboard"};
        PApplet.runSketch(processingargs, Interface.gameOrLeaderboard);
    }
}
