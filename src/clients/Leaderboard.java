import processing.core.PApplet;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.util.Map;

public class Leaderboard {

    PApplet parent;

    String leaderboard;
    Map<String, Player> players;
    ObjectMapper objectMapper = new ObjectMapper();
    String[] headers;
    int[] columnWidths;
    int rowHeight;
    int startX;
    int startY;

    public Leaderboard(PApplet p) {
        parent = p;
    }

    public void setup() {
        headers = new String[]{"Username", "Level", "Victories in a row", "Losses in a row"};
        columnWidths = new int[]{200, 100, 300, 300};
        rowHeight = 60;
        startX = parent.width/2;
        startY = 50;
        System.out.println("AQUI FODASSE");
        try {
            askReceiveLeaderboard();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void drawLeaderboardMenu() {
        parent.background(255);
        parent.fill(0);
        parent.textSize(32);
        parent.textAlign(parent.LEFT, parent.CENTER);

        drawLeaderboard();
    }

    public void drawLeaderboard() {
        startX = (parent.width - columnWidths[0] - columnWidths[1] - columnWidths[2] - columnWidths[3]) / 2;
        startY = 150;
        // Draw headers
        parent.text(headers[0], startX, startY);
        parent.text(headers[1], startX + columnWidths[0], startY);
        parent.text(headers[2], startX + columnWidths[0] + columnWidths[1], startY);
        parent.text(headers[3], startX + columnWidths[0] + columnWidths[1] + columnWidths[2], startY);

        startY += rowHeight;

        // Draw player data
        for (Map.Entry<String, Player> player : players.entrySet()) {
            String[] playerData = {
                    player.getKey(),
                    PApplet.str(player.getValue().level),
                    PApplet.str(player.getValue().victories_in_row),
                    PApplet.str(player.getValue().loses_in_row)
            };
            parent.text(playerData[0], startX + columnWidths[0], startY);
            parent.text(playerData[1], startX + columnWidths[0], startY);
            parent.text(playerData[2], startX + columnWidths[0] + columnWidths[1], startY);
            parent.text(playerData[3], startX + columnWidths[0] + columnWidths[1] + columnWidths[2], startY);

            startY += rowHeight;
        }
    }

    public void askReceiveLeaderboard() throws IOException {
        Interface.wantLeaderboard();
        leaderboard = Interface.receiveLeaderboard();
        GameData gameData = objectMapper.readValue(leaderboard, GameData.class);
        players = gameData.players;
    }
}
