package comm;

import comm.crypt.Crypt;
import comm.crypt.Key;
import comm.message.Message;
import comm.message.request.Connect;
import comm.message.request.Disconnect;
import comm.message.response.MessageFactory;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

/**
 * Created by jimmywest on 2014-12-04.
 */
public abstract class SocketHandler extends Thread{

    private Socket socket;
    private DataOutputStream out;
    private DataInputStream in;
    private Crypt crypt;
    private boolean run = true;

    public SocketHandler(String host, int port, String serverPhrase, String clientPhrase) throws IOException {
        socket = new Socket(host, port);
        crypt = new Crypt(serverPhrase);
        out = new DataOutputStream(socket.getOutputStream());
        in = new DataInputStream(socket.getInputStream());
        connect(clientPhrase);
    }

    public void connect(String clientPhrase) throws IOException {
        Connect connect = new Connect();
        Key ck = new Key(clientPhrase);
        connect.setClientKey(ck);
        send(connect);
        crypt.getKeyRing().setClientKey(ck);
    }

    public void setFileKey(Key key) {
        crypt.getKeyRing().setFileKey(key);
    }

    protected Crypt getCrypt() {
        return crypt;
    }

    @Override
    public void run() {
        byte[] data = new byte[1024];
        while(run) {
            try {
                int count = in.read(data);
                Message message = MessageFactory.createMessage(data, count);
                System.out.println("Got Message: "+message);
                this.handleMessage(message);
            } catch (IOException e) {
                e.printStackTrace();
                run = false;
            }
        }
        try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    protected abstract void handleMessage(Message message);

    public synchronized void send(Message message) throws IOException {
        byte[] cipherText = crypt.encrypt(message.getPlainText());
        out.write(cipherText);
    }

    public void disconnect() throws IOException {
        send(new Disconnect());
        close();
    }

    public void close() {
        this.run = false;
        try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

