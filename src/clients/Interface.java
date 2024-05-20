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
    private static Client_Manager client_manager;

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

    public static void loginUser(String username, String pass){
        client_manager.send("login " + username + " " + pass);
    }

    public void joinUser(String username){
        client_manager.send("join " + username);
    }

    public void logoutUser(String username){
        client_manager.send("logout " + username);
    }
}