import processing.core.PApplet;

import java.io.IOException;
import java.net.Socket;

public class Interface extends PApplet {

    public Socket socket;
    public static String username;

    public static LoginApp loginApp;
    public static Client_Manager client_manager;
    public static Game game;
    public static GameOrLeaderboard gameOrLeaderboard;
    public static Leaderboard leaderboard;

    public static boolean loginMenu = true;
    public static boolean gameOrLeaderboardMenu = false;
    public static boolean leaderboardMenu = false;
    public static boolean gameMenu = false;

    public Interface(Socket s) throws IOException {
        socket = s;
    }

    public static String createUser(String username,String pass) throws IOException {
        client_manager.send("signup " + username + " " + pass);
        return client_manager.receive();
    }

    public static String deleteUser(String username) throws IOException {
        client_manager.send("delete " + username);
        return client_manager.receive();
    }

    public static String loginUser(String username, String pass) throws IOException {
        client_manager.send("login " + username + " " + pass);
        return client_manager.receive();
    }

    public static String logoutUser(String username) throws IOException {
        client_manager.send("logout " + username);
        return client_manager.receive();
    }

    public static void wantPlay(String username){
        client_manager.send("join " + username);
    }

    public static void keyPressed(String username, String key){
        client_manager.send("key " + key + " " + username);
    }

    public static void keyReleased(String username) {
        client_manager.send("released " + username);
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

    public void settings() {
        size(1920, 1080);
        try {
            client_manager = new Client_Manager(socket);
            loginApp = new LoginApp(this);
            game = new Game(this);
            gameOrLeaderboard = new GameOrLeaderboard(this);
            leaderboard = new Leaderboard(this);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void setup() {
        if (loginMenu)
            loginApp.setup();
        else if (gameOrLeaderboardMenu)
            gameOrLeaderboard.setup();
        else if (gameMenu)
            game.setup();
        else if (leaderboardMenu) {
            leaderboard.setup();
        }
    }

    public void draw() {
        if (loginMenu)
            loginApp.drawLogin();
        else if (gameOrLeaderboardMenu)
            gameOrLeaderboard.drawGameOrLeaderboard();
        else if (gameMenu)
            game.drawGameMenu();
        else if (leaderboardMenu)
            leaderboard.drawLeaderboardMenu();

    }

    public void keyPressed() {
        if (loginMenu)
            loginApp.keyPressed();
        else if (gameMenu)
            game.keyPressed();
    }

    public void keyPressed() {
        if (gameMenu)
            game.keyPressed();
    }    

    public void mousePressed() {
        if (loginMenu) {
            try {
                loginApp.mousePressed();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        else if (gameOrLeaderboardMenu) {
            try {
                gameOrLeaderboard.mousePressed();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public void run() {
        String[] processingargs = {"Interface"};
        PApplet.runSketch(processingargs, this);
    }
}
