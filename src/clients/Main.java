import java.io.PrintWriter;
import java.net.Socket;

public class Main {
    public static void main(String[] args) {
        /*
        try{
            Socket sock = new Socket("localhost", 2);
            Client_Manager tcp = new Client_Manager(sock);
            ArrayList<Jogador> player = new ArrayList<Jogador>();
            ArrayList<Corpo_Celeste> corpo = new ArrayList<Corpo_Celeste>();
            new Thread(new Menu()).start();
            //...
        }catch(Exception e) {
            System.out.println(e.getMessage());
        }
        */

        Socket socket = new Socket("localhost", 2);
        Interface interf = new Interface(socket);
        interf.LoginApp.setup();
    }
}
