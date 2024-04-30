public class Jogador{
    
    private String name;
    private int nivel;
    private int gameScore;
    private int gamesWon;
    private int gamesLost;
    private int time;
    private int xPos;
    private int yPos;
    private int angle;
    private int gForce;

    public Jogador(){
        this.name = "";
        this.nivel = 1;
        this.gameScore = 0;  
        this.gamesWon = 0;
        this.gamesLost = 0;
        this.time = 120;
    }
}
