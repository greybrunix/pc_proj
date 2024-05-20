import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class Client_Manager {
    private Socket sock;
    private BufferedReader in;
    private PrintWriter out;
    
    
    public Client_Manager(Socket sock) throws IOException {
        this.sock = sock;
        this.in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
        this.out = new PrintWriter(sock.getOutputStream());
    }

    public void send(String message) {
        out.println(message);
        out.flush();
    }

    public String receive() throws IOException {
        String message = in.readLine();
        return message;
    }

    // TODO Change receive to correct arguments
}  
