import processing.core.PApplet;

import java.io.IOException;

public class LoginApp extends PApplet {

    public String userInput = "";
    public String passwordInput = "";

    int inputBoxWidth = 200;
    int inputBoxHeight = 30;
    int buttonWidth = 100;
    int buttonHeight = 40;
    int inputBoxX;
    int inputBoxY;
    int passwordBoxY;
    int buttonX;
    int buttonY;

    float userTextX;
    float userTextY;
    float passwordTextX;
    float passwordTextY;

    boolean isTypingUsername = false;
    boolean isTypingPassword = false;

    public void settings() {
        size(1920, 1080);

        inputBoxX = (width - inputBoxWidth) / 2;
        inputBoxY = height / 2 - inputBoxHeight;
        passwordBoxY = height / 2 + inputBoxHeight;
        buttonX = inputBoxX;
        buttonY = passwordBoxY + inputBoxHeight + 20;

        userTextX = inputBoxX + 5;
        userTextY = inputBoxY + (float) inputBoxHeight / 2;
        passwordTextX = inputBoxX + 5;
        passwordTextY = passwordBoxY + (float) inputBoxHeight / 2;
    }

    public void draw() {
        background(255);
        fill(0);
        textSize(32);
        textAlign(LEFT, CENTER); // Align text to the left, center vertically

        // Draw input box
        fill(255);
        stroke(0);
        rect(inputBoxX, inputBoxY, inputBoxWidth, inputBoxHeight);

        // Display user input
        fill(0); // Set the text color to black
        text(userInput, userTextX, userTextY);

        if (isTypingUsername) {
            drawCursor(userTextX + textWidth(userInput), userTextY);
        }

        // Draw password input box
        fill(255);
        stroke(0);
        rect(inputBoxX, passwordBoxY, inputBoxWidth, inputBoxHeight);
        fill(0);
        text(hidePassword(passwordInput), passwordTextX, passwordTextY);

        if (isTypingPassword) {
            drawCursor(passwordTextX + textWidth(hidePassword(passwordInput)), passwordTextY);
        }

        // Draw login button
        fill(200);
        rect(buttonX, buttonY, buttonWidth, buttonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Login", buttonX + buttonWidth / 2, buttonY + buttonHeight / 2);

        // Draw create account button
        fill(200);
        rect(buttonX, buttonY+50, buttonWidth+110, buttonHeight);
        fill(0);
        textAlign(CENTER, CENTER);
        text("Create Account", buttonX + (buttonWidth+110) / 2, buttonY + 50 + buttonHeight / 2);
    }

    String hidePassword(String password) {
        StringBuilder hidden = new StringBuilder();
        for (int i = 0; i < password.length(); i++) {
            hidden.append('*');
        }
        return hidden.toString();
    }

    void drawCursor(float cursorX, float cursorY) {
        // Draw cursor
        stroke(0);
        line(cursorX, cursorY - 16, cursorX, cursorY + 16);
    }

    public void keyPressed() {
        if (isTypingUsername) {
            if (key != ENTER && key != BACKSPACE) {
                if (textWidth(userInput) < inputBoxWidth - 20) { // Check if text exceeds box width
                    userInput += key;
                }
            } else if (key == BACKSPACE && !userInput.isEmpty()) {
                userInput = userInput.substring(0, userInput.length() - 1);
            }
        } else if (isTypingPassword) {
            if (key != ENTER && key != BACKSPACE) {
                if (textWidth(passwordInput) < inputBoxWidth - 20) { // Check if text exceeds box width
                    passwordInput += key;
                }
            } else if (key == BACKSPACE && !userInput.isEmpty()) {
                passwordInput = passwordInput.substring(0, passwordInput.length() - 1);
            }
        }
    }

    public void mousePressed() {
        if (mouseX >= inputBoxX && mouseX <= inputBoxX + inputBoxWidth &&
                mouseY >= inputBoxY && mouseY <= inputBoxY + inputBoxHeight) {
            isTypingUsername = true;
            isTypingPassword = false;
        } else if (mouseX >= inputBoxX && mouseX <= inputBoxX + inputBoxWidth &&
                mouseY >= passwordBoxY && mouseY <= passwordBoxY + inputBoxHeight) {
            isTypingUsername = false;
            isTypingPassword = true;
        } else if (mouseX >= buttonX && mouseX <= buttonX + buttonWidth &&
                mouseY >= buttonY && mouseY <= buttonY + buttonHeight) {
            Interface.loginUser(userInput, passwordInput); // TODO check if login was a success and change page to game
            Interface.gameOrLeaderboard.run(userInput);
        } else if (mouseX >= buttonX && mouseX <= buttonX + buttonWidth + 110 &&
                mouseY >= buttonY && mouseY <= buttonY + buttonHeight + 50) {
            Interface.createUser(userInput, passwordInput); // TODO check if login was a success and change page to game
            Interface.gameOrLeaderboard.run(userInput);
        } else {
            isTypingUsername = false;
            isTypingPassword = false;
        }
    }

    public void run() {
        String[] processingargs = {"LoginApp"};
        PApplet.runSketch(processingargs, Interface.loginApp);
    }
}