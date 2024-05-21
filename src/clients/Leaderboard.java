import processing.core.PApplet;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.Map;

public class Leaderboard extends PApplet {

    String leaderboard;
    Map<String, Player> players;
    ObjectMapper objectMapper = new ObjectMapper();
    String[] headers = {"Username", "Level", "Victories in a row", "Losses in a row"};
    int[] columnWidths = {200, 100, 300, 300};
    int rowHeight = 60;
    int startX = width/2;
    int startY = 50;

    public void settings() {
        size(1920, 1080);
    }

    public void draw() {
        background(255);
        fill(0);
        textSize(32);
        textAlign(LEFT, CENTER);

        drawLeaderboard();
    }

    public void drawLeaderboard() {
        startX = (width - columnWidths[0] - columnWidths[1] - columnWidths[2] - columnWidths[3]) / 2;
        startY = 150;
        // Draw headers
        text(headers[0], startX, startY);
        text(headers[1], startX + columnWidths[0], startY);
        text(headers[2], startX + columnWidths[0] + columnWidths[1], startY);
        text(headers[3], startX + columnWidths[0] + columnWidths[1] + columnWidths[2], startY);

        startY += rowHeight;

        // Draw player data
        for (Map.Entry<String, Player> player : players.entrySet()) {
            String[] playerData = {
                    player.getKey(),
                    str(player.getValue().level),
                    str(player.getValue().victories_in_row),
                    str(player.getValue().loses_in_row)
            };
            text(playerData[0], startX + columnWidths[0], startY);
            text(playerData[1], startX + columnWidths[0], startY);
            text(playerData[2], startX + columnWidths[0] + columnWidths[1], startY);
            text(playerData[3], startX + columnWidths[0] + columnWidths[1] + columnWidths[2], startY);

            startY += rowHeight;
        }
    }

    public void askReceiveLeaderboard() throws IOException {
        //Interface.wantLeaderboard();
        //leaderboard = Interface.receiveLeaderboard();
        leaderboard = "{" +
                "\"players\": {" +
                "\"username\": {" +
                "\"level\": 50," +
                "\"victories_in_row\": 30," +
                "\"loses_in_row\": 0," +
                "\"x\": 0," +
                "\"y\": 0," +
                "\"diameter\": 0," +
                "\"targetX\": 0," +
                "\"targetY\": 0," +
                "\"angle\": 0," +
                "\"lineLenght\": 0," +
                "\"lineEndX\": 0," +
                "\"lineEndY\": 0," +
                "\"targetAngle\": 0," +
                "\"easingAngle\": 0," +
                "\"r\": 0," +
                "\"g\": 0," +
                "\"b\": 0," +
                "\"fuel\": 0," +
                "\"waitingGame\": false," +
                "\"game\": false," +
                "\"gameOver\": false" +
                "}," +
                "\"username1\": {" +
                "\"level\": 10," +
                "\"victories_in_row\": 0," +
                "\"loses_in_row\": 5," +
                "\"x\": 0," +
                "\"y\": 0," +
                "\"diameter\": 0," +
                "\"targetX\": 0," +
                "\"targetY\": 0," +
                "\"angle\": 0," +
                "\"lineLenght\": 0," +
                "\"lineEndX\": 0," +
                "\"lineEndY\": 0," +
                "\"targetAngle\": 0," +
                "\"easingAngle\": 0," +
                "\"r\": 0," +
                "\"g\": 0," +
                "\"b\": 0," +
                "\"fuel\": 0," +
                "\"waitingGame\": false," +
                "\"game\": false," +
                "\"gameOver\": false" +
                "}" +
                "}" +
                "}";
        GameData gameData = objectMapper.readValue(leaderboard, GameData.class);
        players = gameData.players;
        run();
    }

    public void run() {
        String[] processingargs = {"Leaderboard"};
        PApplet.runSketch(processingargs, Interface.leaderboard);
    }
}
