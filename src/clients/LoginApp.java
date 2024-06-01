import processing.core.PApplet;

import java.io.IOException;

public class LoginApp {

    PApplet parent;
    public String userInput = "";
    public String passwordInput = "";

    int inputBoxWidth;
    int inputBoxHeight;
    int buttonWidth;
    int buttonHeight;
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

    public LoginApp(PApplet p) {
        parent = p;
    }

    public void setup() {
        inputBoxWidth = 200;
        inputBoxHeight = 30;
        buttonWidth = 100;
        buttonHeight = 40;

        inputBoxX = (parent.width - inputBoxWidth) / 2;
        inputBoxY = parent.height / 2 - inputBoxHeight;
        passwordBoxY = parent.height / 2 + inputBoxHeight;
        buttonX = inputBoxX;
        buttonY = passwordBoxY + inputBoxHeight + 20;

        userTextX = inputBoxX + 5;
        userTextY = inputBoxY + (float) inputBoxHeight / 2;
        passwordTextX = inputBoxX + 5;
        passwordTextY = passwordBoxY + (float) inputBoxHeight / 2;
    }

    public void drawLogin() {
        parent.background(255);
        parent.fill(0);
        parent.textSize(32);
        parent.textAlign(parent.LEFT, parent.CENTER); // Align text to the left, center vertically

        // Draw input box
        parent.fill(255);
        parent.stroke(0);
        parent.rect(inputBoxX, inputBoxY, inputBoxWidth, inputBoxHeight);

        // Display user input
        parent.fill(0); // Set the text color to black
        parent.text(userInput, userTextX, userTextY);

        if (isTypingUsername) {
            drawCursor(userTextX + parent.textWidth(userInput), userTextY);
        }

        // Draw password input box
        parent.fill(255);
        parent.stroke(0);
        parent.rect(inputBoxX, passwordBoxY, inputBoxWidth, inputBoxHeight);
        parent.fill(0);
        parent.text(hidePassword(passwordInput), passwordTextX, passwordTextY);

        if (isTypingPassword) {
            drawCursor(passwordTextX + parent.textWidth(hidePassword(passwordInput)), passwordTextY);
        }

        // Draw login button
        parent.fill(200);
        parent.rect(buttonX, buttonY, buttonWidth, buttonHeight);
        parent.fill(0);
        parent.textAlign(parent.CENTER, parent.CENTER);
        parent.text("Login", buttonX + buttonWidth / 2, buttonY + buttonHeight / 2);

        // Draw create account button
        parent.fill(200);
        parent.rect(buttonX, buttonY+50, buttonWidth+110, buttonHeight);
        parent.fill(0);
        parent.textAlign(parent.CENTER, parent.CENTER);
        parent.text("Create Account", buttonX + (buttonWidth+110) / 2, buttonY + 50 + buttonHeight / 2);
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
        parent.stroke(0);
        parent.line(cursorX, cursorY - 16, cursorX, cursorY + 16);
    }

    public void keyPressed() {
        if (isTypingUsername) {
            if (parent.key != parent.ENTER && parent.key != parent.BACKSPACE) {
                if (parent.textWidth(userInput) < inputBoxWidth - 20) { // Check if text exceeds box width
                    userInput += parent.key;
                }
            } else if (parent.key == parent.BACKSPACE && !userInput.isEmpty()) {
                userInput = userInput.substring(0, userInput.length() - 1);
            }
        } else if (isTypingPassword) {
            if (parent.key != parent.ENTER && parent.key != parent.BACKSPACE) {
                if (parent.textWidth(passwordInput) < inputBoxWidth - 20) { // Check if text exceeds box width
                    passwordInput += parent.key;
                }
            } else if (parent.key == parent.BACKSPACE && !passwordInput.isEmpty()) {
                passwordInput = passwordInput.substring(0, passwordInput.length() - 1);
            }
        }
    }

    public void mousePressed() throws IOException {
        if (parent.mouseX >= inputBoxX && parent.mouseX <= inputBoxX + inputBoxWidth &&
                parent.mouseY >= inputBoxY && parent.mouseY <= inputBoxY + inputBoxHeight) {
            isTypingUsername = true;
            isTypingPassword = false;
        } else if (parent.mouseX >= inputBoxX && parent.mouseX <= inputBoxX + inputBoxWidth &&
                parent.mouseY >= passwordBoxY && parent.mouseY <= passwordBoxY + inputBoxHeight) {
            isTypingUsername = false;
            isTypingPassword = true;
        } else if (parent.mouseX >= buttonX && parent.mouseX <= buttonX + buttonWidth &&
                parent.mouseY >= buttonY && parent.mouseY <= buttonY + buttonHeight) {
            String res = Interface.loginUser(userInput, passwordInput); // TODO check if login was a success and change page to game
            if (res.equals("ok")) {
                parent.setup();
                Interface.loginMenu = false;
                Interface.gameOrLeaderboardMenu = true;
                Interface.username = userInput;
            }
            userInput = "";
            passwordInput = "";
            isTypingUsername = false;
            isTypingPassword = false;
        } else if (parent.mouseX >= buttonX && parent.mouseX <= buttonX + buttonWidth + 110 &&
                parent.mouseY >= buttonY && parent.mouseY <= buttonY + buttonHeight + 50) {
            String res = Interface.createUser(userInput, passwordInput); // TODO check if login was a success and change page to game
            if (res.equals("ok")) {
                Interface.loginMenu = false;
                Interface.gameOrLeaderboardMenu = true;
                Interface.username = userInput;
                System.out.println();
                parent.setup();
            }
            userInput = "";
            passwordInput = "";
            isTypingUsername = false;
            isTypingPassword = false;
        } else {
            isTypingUsername = false;
            isTypingPassword = false;
        }
    }
}