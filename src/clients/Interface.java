import processing.core.PApplet;
import java.io.IOException;
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
        public void settings() {
            size(1920, 1080);
            //background(128);
            //textAlign(CENTER, CENTER);
        }
        public void setup() {
        }
        public void draw() {
            background(255);
            fill(0);
            textSize(32);
            textAlign(CENTER, CENTER);
            text("POR FAVOR APARECE", (float) width /2, (float) height /2);
        }
        public void run() {
            String[] processingargs = {"Interface"};
            PApplet.runSketch(processingargs, loginApp);
        }
    }
}