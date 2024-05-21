import java.io.IOException;
import java.net.Socket;

public class Interface {

    public static LoginApp loginApp;
    public static Client_Manager client_manager;
    public static Game game;
    public static GameOrLeaderboard gameOrLeaderboard;
    public static Leaderboard leaderboard;

    public Interface(Socket socket) throws IOException {
        try {
            client_manager = new Client_Manager(socket);
            loginApp = new LoginApp();
            game = new Game();
            gameOrLeaderboard = new GameOrLeaderboard();
            leaderboard = new Leaderboard();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void createUser(String username,String pass){
        client_manager.send("signup " + username +" " + pass);
    }

    public static void deleteUser(String username,String pass){
        client_manager.send("delete " + username + " " + pass);
    }

    public static void loginUser(String username, String pass){
        client_manager.send("login " + username + " " + pass);
    }

    public static void joinUser(String username){
        client_manager.send("join " + username);
    }

    public static void logoutUser(String username){
        client_manager.send("logout " + username);
    }

    public static void wantPlay(String username){
        client_manager.send("play " + username);
    }

    public static void keyPressed(String username, String key){
        client_manager.send("key " + key + " " + username);
    }

    public static String receiveData() throws IOException {
        return client_manager.receive();
    }

    public static void wantLeaderboard(){
        client_manager.send("leaderboard");
    }

    public static String receiveLeaderboard() throws IOException {
        return client_manager.receive();
    }

    // TODO Add option to delete account and to log out
}