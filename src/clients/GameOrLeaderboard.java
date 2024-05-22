import processing.core.PApplet;

import java.io.IOException;

public class GameOrLeaderboard {

    PApplet parent;

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

    public GameOrLeaderboard(PApplet p) {
        parent = p;
        setup();
    }

    public void setup() {
        gameButtonX = (float) (parent.width - gameButtonWidth - 500) / 2;
        gameButtonY = (float) (parent.height - gameButtonHeight) / 2;
        leaderboardButtonX = (float) (parent.width - leaderboardButtonWidth + 500) / 2;
        leaderboardButtonY = (float) (parent.height - leaderboardButtonHeight) / 2;
        logoutButtonX = 20;
        logoutButtonY = 1000;
        deleteAccountButtonX = 1650;
        deleteAccountButtonY = 1000;
    }

    public void drawGameOrLeaderboard() {
        parent.background(184);
        parent.fill(0);
        parent.textSize(32);
        parent.textAlign(parent.LEFT, parent.CENTER);

        // Draw game button
        parent.fill(255);
        parent.rect(gameButtonX, gameButtonY, gameButtonWidth, gameButtonHeight);
        parent.fill(0);
        parent.textAlign(parent.CENTER, parent.CENTER);
        parent.text("Play Game", gameButtonX + (float) gameButtonWidth /2,
                gameButtonY + (float) gameButtonHeight /2);

        // Draw leaderboard button
        parent.fill(255);
        parent.rect(leaderboardButtonX, leaderboardButtonY, leaderboardButtonWidth, leaderboardButtonHeight);
        parent.fill(0);
        parent.textAlign(parent.CENTER, parent.CENTER);
        parent.text("Leaderboard", leaderboardButtonX + (float) leaderboardButtonWidth / 2,
                leaderboardButtonY + (float) leaderboardButtonHeight / 2);

        // Draw logout button
        parent.fill(255);
        parent.rect(logoutButtonX, logoutButtonY, logoutButtonWidth, logoutButtonHeight);
        parent.fill(0);
        parent.textAlign(parent.CENTER, parent.CENTER);
        parent.text("Logout", logoutButtonX + (float) logoutButtonWidth / 2,
                logoutButtonY + (float) logoutButtonHeight / 2);

        // Draw delete account button
        parent.fill(255);
        parent.rect(deleteAccountButtonX, deleteAccountButtonY, deleteAccountButtonWidth, deleteAccountButtonHeight);
        parent.fill(0);
        parent.textAlign(parent.CENTER, parent.CENTER);
        parent.text("Delete Account", deleteAccountButtonX + (float) deleteAccountButtonWidth / 2,
                deleteAccountButtonY + (float) deleteAccountButtonHeight / 2);
    }

    public void mousePressed() {
        if (parent.mouseX >= gameButtonX && parent.mouseX <= gameButtonX + gameButtonWidth &&
                parent.mouseY >= gameButtonY && parent.mouseY <= gameButtonY + gameButtonHeight) {
            try {
                Interface.game.waitGame(Interface.username);
            } catch (IOException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        } else if (parent.mouseX >= leaderboardButtonX && parent.mouseX <= leaderboardButtonX + leaderboardButtonWidth &&
                    parent.mouseY >= leaderboardButtonY && parent.mouseY <= leaderboardButtonY + leaderboardButtonHeight) {
            try {
                Interface.leaderboard.askReceiveLeaderboard();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        } else if (parent.mouseX >= logoutButtonX && parent.mouseX <= logoutButtonX + logoutButtonWidth &&
                parent.mouseY >= logoutButtonY && parent.mouseY <= logoutButtonY + logoutButtonHeight) {
            Interface.logoutUser(Interface.username);
            Interface.gameOrLeaderboardMenu = false;
            Interface.loginMenu = true;
            Interface.username = null;
        } else if (parent.mouseX >= deleteAccountButtonX && parent.mouseX <= deleteAccountButtonX + deleteAccountButtonWidth &&
        parent.mouseY >= deleteAccountButtonY && parent.mouseY <= deleteAccountButtonY + deleteAccountButtonWidth) {
            Interface.deleteUser(Interface.username);
            Interface.gameOrLeaderboardMenu = false;
            Interface.loginMenu = true;
            Interface.username = null;
        }
    }
}
