import processing.core.PApplet;
import processing.core.PGraphics;
import processing.core.PFont;
import processing.core.PSketch;
import processing.core.PStyle;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

public class Interface {

    public LoginApp loginApp;
    private Client_Manager client_manager;

    public Interface(Socket socket) throws IOException {
        try {
            client_manager = new Client_Manager(socket);
            loginApp = new LoginApp();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void createUser(String username,String pass){
        client_manager.send("signup " + username +" " + pass);
    }

    public void deleteUser(String username,String pass){
        client_manager.send("delete " + username + " " + pass);
    }

    public void loginUser(String username,String pass){
        client_manager.send("login " + username + " " + pass);
    }

    public void joinUser(String username){
        client_manager.send("join " + username);
    }

    public void logoutUser(String username){
        client_manager.send("logout " + username);
    }

    public class LoginApp extends PApplet {
        private String userInput = "";
        int inputBoxWidth = 200;
        int inputBoxHeight = 30;
        int inputBoxX;
        int inputBoxY;
        float textX;
        float textY;
        boolean isTyping = false;

        public void settings() {
            size(1920, 1080);
            inputBoxX = (width - inputBoxWidth) / 2;
            textX = inputBoxX;
            inputBoxY = height / 2;
        }

        public void setup() {
        }

        public void draw() {
            background(255);
            fill(0);
            textSize(32);
            textAlign(CENTER, CENTER);

            // Draw input box
            fill(255);
            stroke(0);
            rect(inputBoxX, inputBoxY, inputBoxWidth, inputBoxHeight);

            // Calculate text position
            float textX = inputBoxX + 10; // Initial x position for the text
            float textY = inputBoxY + inputBoxHeight / 2; // Center y position for the text
            if (textWidth(userInput) > inputBoxWidth - 20) {
                // If text width exceeds input box width, adjust x position
                textX = inputBoxX + inputBoxWidth - 20 - textWidth(userInput);
            }

            // Display user input
            fill(0); // Set the text color to black
            text(userInput, textX, textY);

            if (isTyping) {
                drawCursor(textX, textY);
            }
        }

        void drawCursor(float textX, float textY) {
            // Calculate cursor position
            float cursorX = textX + textWidth(userInput); // Adjust cursor position
            float cursorY = textY - 16; // Adjust cursor position

            // Draw cursor
            stroke(0);
            line(cursorX, cursorY, cursorX, cursorY + 32);
        }

        public void keyPressed() {
            if (isTyping) {
                if (key != ENTER && key != BACKSPACE) {
                    if (textWidth(userInput) < inputBoxWidth - 20) { // Check if text exceeds box width
                        userInput += key;
                    }
                } else if (key == BACKSPACE && !userInput.isEmpty()) {
                    userInput = userInput.substring(0, userInput.length() - 1);
                } else if (key == ENTER) {
                    // Process the entered username here
                    println("Entered username: " + userInput);
                    userInput = ""; // Reset userInput for the next input
                }
            }
        }

        public void mousePressed() {
            if (mouseX >= inputBoxX && mouseX <= inputBoxX + inputBoxWidth &&
                    mouseY >= inputBoxY && mouseY <= inputBoxY + inputBoxHeight) {
                isTyping = true;
            } else {
                isTyping = false;
            }
        }

        public void run() {
            String[] processingargs = {"Interface"};
            PApplet.runSketch(processingargs, loginApp);
        }
    }
}