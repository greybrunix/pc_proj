import processing.core.PApplet;
import java.net.Socket;

public class Interface { 

    private Client_Manager client_manager;
    public Interface(Socket socket) {
        this.client_manager = new Client_Manager(socket);
    }

    public void createUser(String username,String pass){
        client_manager.send("signup " + username +" " + pass);
    }

    public void deleteUser(String username,String pass){
        client_manager.send("delete " + username + " " + pass);
    }

    public void loginUser(String username,String pass){
        client_manager.send("delete " + username + " " + pass);
    }

    public void joinUser(String username){
        client_manager.send("join " + username);
    }

    public void logoutUser(String username){
        client_manager.send("logout " + username);
    }

    public class LoginApp extends PApplet {
        
         public void setup() {
            size(1920, 1080);
            background(128);
            textAlign(CENTER, CENTER);
        }
    }

}
